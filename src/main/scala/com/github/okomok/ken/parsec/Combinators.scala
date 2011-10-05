

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


private[parsec] trait Combinators[s, u, n[+_]] { this: ParsecTOp[s, u, n] =>
    import parsecMonad._

    def choice[a](ps: List[ParsecT[s, u, n, a]]): ParsecT[s, u, n, a] = List.foldr[apply1[a], apply1[a]](op_<|>)(mzero)(ps)

    def option[a](x: a)(p: ParsecT[s, u, n, a]): ParsecT[s, u, n, a] = p <|> `return`(x)

    def optionMaybe[a](x: a)(p: ParsecT[s, u, n, a]): ParsecT[s, u, n, Maybe[a]] = option[Maybe[a]](Nothing)(liftM(Just(_: a))(p))

    lazy val optional: ParsecT[s, u, n, Any] => ParsecT[s, u, n, Unit] = p => ( for { _ <- p } yield () ) <|> `return`()

    // open p close
    def between[a](open: ParsecT[s, u, n, _])(close: ParsecT[s, u, n, _])(p: ParsecT[s, u, n, a]): ParsecT[s, u, n, a] = for { _ <- open; x <- p; _ <- close } yield x

    // p+ (result abandoned)
    lazy val skipMany1: ParsecT[s, u, n, Any] => ParsecT[s, u, n, Unit] = p => for { _ <- p; * <- skipMany(p) } yield *

    // p+
    def many1[a](p: ParsecT[s, u, n, a]): ParsecT[s, u, n, List[a]] = for { x <- p; xs <- many(p) } yield (x :: xs)

    // p (sep p)
    def sepBy1[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = for { x <- p; xs <- many(sep >> p) } yield (x :: xs)

    def sepBy[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = sepBy1(p)(sep) <|> `return`(Nil)

    def sepEndBy1[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = {
        for {
            x <- p
            * <- ( for { _ <- sep; xs <- sepEndBy(p)(sep) } yield (x :: xs) ) <|> `return`(List(x))
        } yield *
    }

    def sepEndBy[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = sepEndBy1(p)(sep) <|> `return`(Nil)

    // (p sep)+
    def endBy1[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = many1 { for { x <- p; _ <- sep } yield x }

    // (p sep)*
    def endBy[a](p: ParsecT[s, u, n, a])(sep: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = many { for { x <- p; _ <- sep } yield x }

    // p{n}
    def count[a](n: Int)(p: ParsecT[s, u, n, a]): ParsecT[s, u, n, List[a]] = {
        if (n <= 0) `return`(Nil)
        else sequence(List.replicate(n)(p))
    }

    // folding with seed
    def chainr[a](p: ParsecT[s, u, n, a])(op: ParsecT[s, u, n, a => a => a])(x: a): ParsecT[s, u, n, a] = chainr1(p)(op) <|> `return`(x)

    // folding with seed
    def chainl[a](p: ParsecT[s, u, n, a])(op: ParsecT[s, u, n, a => a => a])(x: a): ParsecT[s, u, n, a] = chainl1(p)(op) <|> `return`(x)

    // folding without seed
    def chainr1[a](p: ParsecT[s, u, n, a])(op: ParsecT[s, u, n, a => a => a]): ParsecT[s, u, n, a] = {
        def rest(x: a): ParsecT[s, u, n, a] = ( for { f <- op; y <- scan } yield f(x)(y) ) <|> `return`(x)
        lazy val scan: ParsecT[s, u, n, a] = for { x <- p; * <- rest(x) } yield *
        scan
    }

    // folding without seed
    def chainl1[a](p: ParsecT[s, u, n, a])(op: ParsecT[s, u, n, a => a => a]): ParsecT[s, u, n, a] = {
        def rest(x: a): ParsecT[s, u, n, a] = ( for { f <- op; y <- p; z <- rest(f(x)(y)) } yield z ) <|> `return`(x)
        for { x <- p; * <- rest(x) } yield *
    }

    def anyToken[t](implicit si: Stream[s, n, t], sj: Show[t]): ParsecT[s, u, n, t] = {
        tokenPrim[t, t](sj.show)(pos => _tok => _toks => pos)(Just(_))
    }

    def eof[t](implicit si: Stream[s, n, t], sj: Show[t]): ParsecT[s, u, n, Unit] = notFollowedBy(anyToken[t]) <#> "end of input"

    // negative lookahead
    def notFollowedBy[a](p: ParsecT[s, u, n, a])(implicit sj: Show[a]): ParsecT[s, u, n, Unit] = {
        `try` {
            ( for { c <- p; _ <- unexpected(sj.show(c)) } yield () ) <|> `return`()
        }
    }

    // star-until
    def manyTill[a](p: ParsecT[s, u, n, a])(end: ParsecT[s, u, n, _]): ParsecT[s, u, n, List[a]] = {
        lazy val scan: ParsecT[s, u, n, List[a]] = {
            ( for { _ <- end } yield Nil.of[a] ) <|> ( for { x <- p; xs <- scan } yield (x :: xs) )
        }
        scan
    }

    // positive lookahead
    def lookAhead[a](p: ParsecT[s, u, n, a]): ParsecT[s, u, n, a] = {
        for { state <- getParserState; x <- p; _ <- setParserState(state) } yield x
    }
}
