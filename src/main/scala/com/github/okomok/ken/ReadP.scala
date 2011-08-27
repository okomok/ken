

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


// See: http://www.cse.chalmers.se/edu/course/afp/Papers/parser-claessen.pdf


package com.github.okomok
package ken


trait ReadP[+a] extends Kind.constThis {
    def apply[b](k: a => ReadP.P[b]): ReadP.P[b]
}


object ReadP extends MonadPlus[ReadP] with ThisIsInstance {
    // The P type
    //
    sealed abstract class P[+a] extends Up[P[a]] with Kind.constThis

    final case class Get[a](_1: Char => P[a]) extends P[a]
    final case class Look[a](_2: String_ => P[a]) extends P[a]
    final case object Fail extends P[Nothing]
    final case class Result[a](_1: a, _2: P[a]) extends P[a]
    final case class Final[a](_1: List[(a, String_)]) extends P[a] {
        Predef.require { not(List.`null`(_1)) }
    }

    object Get extends ReadP[Char]
    object Look extends ReadP[String_]

    object P extends MonadPlus[P] with ThisIsInstance {
        // Overrides
        //
        // Monad
        private type m[+a] = P[a]
        override def `return`[a](x: Lazy[a]): m[a] = Result(x, Fail)
        override def op_>>=[a, b](p: m[a])(k: a => m[b]): m[b] = p match {
            case Get(f) => Get(c => f(c) >>= k)
            case Look(f) => Look(s => f(s) >>= k)
            case Fail => Fail
            case Result(x, p) => k(x) _mplus_ (p >>= k)
            case Final(r) => `final` {
                for { (x, s) <- r; ys_ <- run(k(x))(s) } yield ys_
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = Fail
        override def mplus[a](p: m[a])(q: Lazy[m[a]]): m[a] = (p, q.!) match {
            // parallel processing
            case (Get(f1), Get(f2)) => Get(c => f1(c) _mplus_ f2(c))
            case (Result(x, p), q) => Result(x, p _mplus_ q)
            case (p, Result(x, q)) => Result(x, p _mplus_ q)
            case (Fail, p) => p
            case (p, Fail) => p
            case (Final(r), Final(t)) => Final(r ::: t)
            case (Final(r), Look(f)) => Look(s => Final(r ::: run(f(s))(s)))
            case (Final(r), p) => Look(s => Final(r ::: run(p)(s)))
            case (Look(f), Final(r)) => Look(s => Final(run(f(s))(s) ::: r))
            case (p, Final(r)) => Look(s => Final(run(p)(s) ::: r))
            case (Look(f), Look(g)) => Look(s => f(s) _mplus_ g(s))
            case (Look(f), p) => Look(s => f(s) _mplus_ p)
            case (p, Look(f)) => Look(s => p _mplus_ f(s))
        }

        // Operations
        //
        def `final`[a](r: List[(a, String_)]): P[a] = r match {
            case Nil => Fail
            case r => Final(r)
        }

        def run[a](p: P[a]): ReadS[a] = s => (p, s) match {
            case (Get(f), c :: s) => run(f(c))(s)
            case (Look(f), s) => run(f(s))(s)
            case (Result(x, p), s) => (x, s) :: run(p)(s)
            case (Final(r), _) => r
            case _ => Nil
        }
    }

    // ReadS
    //
    type ReadS[+a] = String_ => List[(a, String_)]

    // Overrides
    //
    // Functor
    private type f[+a] = ReadP[a]
    override def fmap[a, b](h: a => b)(f: f[a]): f[b] = new ReadP[b] {
        override def apply[c](k: b => P[c]): P[c] = f(k compose h)
    }
    // Monad
    private type m[+a] = ReadP[a]
    override def `return`[a](x: Lazy[a]): m[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]) = k(x)
    }
    override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = new ReadP[b] {
        override def apply[c](k: b => P[c]): P[c] = m(a => f(a)(k))
    }
    // MonadPlus
    override def mzero: m[Nothing] = pfail
    override def mplus[a](f1: m[a])(f2: Lazy[m[a]]): m[a] = f1 +++ f2

    // Operations
    //
    val get: ReadP[Char] = Get

    val look: ReadP[String_] = Look

    def pfail[a]: ReadP[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]): P[b] = Fail
    }

    def op_+++[a](f1: ReadP[a])(f2: Lazy[ReadP[a]]): ReadP[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]): P[b] = f1(k) _mplus_ f2(k)
    }

    sealed class Op_+++[a](f1: ReadP[a]) {
        def +++(f2: Lazy[ReadP[a]]): ReadP[a] = op_+++(f1)(f2)
    }
    implicit def +++[a](f1: ReadP[a]): Op_+++[a] = new Op_+++(f1)

    def op_<++[a](f: ReadP[a])(q: Lazy[ReadP[a]]): ReadP[a] = {
        def discard(n: Int): ReadP[Unit] = {
            if (n == 0) `return`()
            else get >> discard(n-1)
        }
        def probe(p: P[a])(s: String_)(n: Int): ReadP[a] = (p, s) match {
            case (Get(f), c :: s) => probe(f(c))(s)(n+1)
            case (Look(f), s) => probe(f(s))(s)(n)
            case (Result(_, _), _) => discard(n) >> new ReadP[a] {
                override def apply[b](k: a => P[b]): P[b] = p >>= k
            }
            case (Final(_), _) => new ReadP[a] {
                override def apply[b](k: a => P[b]): P[b] = p >>= k
            }
            case _ => q
        }
        for {
            s <- look
            * <- probe(f(P.`return`[a]))(s)(0)
        } yield *
    }

    sealed class Op_<++[a](f: ReadP[a]) {
        def <++(q: Lazy[ReadP[a]]): ReadP[a] = op_<++(f)(q)
    }
    implicit def <++[a](f: ReadP[a]): Op_<++[a] = new Op_<++(f)

    def gather[a](m: ReadP[a]): ReadP[(String_, a)] = {
        def gath[b](l: String_ => String_)(p: P[String_ => P[b]]): P[b] = p match {
            case Get(f) => Get(c => gath(l compose List.op_::(c))(f(c)))
            case Fail => Fail
            case Look(f) => Look(s => gath(l)(f(s)))
            case Result(k, p) => P.mplus(k(l(Nil)))(gath(l)(p))
            case Final(_) => error("do not use readS_to_P in gather!")
        }
        new ReadP[(String_, a)] {
            override def apply[b](k: Tuple2[String_, a] => P[b]): P[b] = {
                gath(id)(m(a => P.`return`((s: String_) => k(s, a))))
            }
        }
    }

    val satisfy: (Char => Bool) => ReadP[Char] = p => {
        for {
            c <- get
            * <- if (p(c)) `return`(c) else pfail[Char]
        } yield *
    }

    val char: Char => ReadP[Char] = c => satisfy(c == _)

    val eof: ReadP[Unit] = {
        for {
            s <- look
            * <- if (List.`null`(s)) `return`() else pfail[Unit]
        } yield *
    }

    val string: String_ => ReadP[String_] = _this => {
        def scan(xs: String_)(ys: String_): ReadP[String_] = (xs, ys) match {
            case (Nil, _) => `return`(_this)
            case (x :: xs, y :: ys) => for { _ <- get; * <- scan(xs)(ys) } yield *
            case _ => pfail
        }
        for { s <- look; * <- scan(_this)(s) } yield *
    }

    val munch: (Char => Bool) => ReadP[String_] = p => {
        def scan(s: String_): ReadP[String_] = s match {
            case c :: cs if (p(c)) => for { _ <- get; s <- scan(cs) } yield (c :: s)
            case _ => `return`(Nil)
        }
        for {
            s <- look
            * <- scan(s)
        } yield *
    }

    val munch1: (Char => Bool) => ReadP[String_] = p => {
        for {
            c <- get
            * <- if (p(c)) ( for { s <- munch(p) } yield (c :: s) ) else pfail
        } yield *
    }

    def choice[a](ps: List[ReadP[a]]): ReadP[a] = ps match {
        case Nil => pfail
        case p !:: Nil => p
        case p :: ps => p +++ choice(ps)
    }

    val skipSpaces: ReadP[Unit] = {
        def skip(s: String_): ReadP[Unit] = s match {
            case c :: s if Char.isSpace(c) => for { _ <- get; * <- skip(s) } yield *
            case _ => `return`()
        }
        for {
            s <- look
            * <- skip(s)
        } yield *
    }

    def count[a](n: Int)(p: ReadP[a]): ReadP[List[a]] = sequence(List.replicate(n)(p))

    def between[open, close, a](open: ReadP[open])(close: ReadP[close])(p: ReadP[a]): ReadP[a] = {
        for {
            _ <- open
            x <- p
            _ <- close
        } yield x
    }

    def option[a](x: a)(p: ReadP[a]): ReadP[a] = p +++ `return`(x)
    def optional_[a](p: ReadP[a]): ReadP[Unit] = (p >> `return`()) +++ `return`()

    override def many[a](p: ReadP[a]): ReadP[List[a]] = `return`(Nil.of[a]) +++ many1(p)
    def many1[a](p: ReadP[a]): ReadP[List[a]] = liftM2(List.op_!::[a])(p)(many(p))

    def skipMany[a](p: ReadP[a]): ReadP[Unit] = many(p) >> `return`()
    def skipMany1[a](p: ReadP[a]): ReadP[Unit] = p >> skipMany(p)

    def sepBy[a, sep](p: ReadP[a])(sep: ReadP[sep]): ReadP[List[a]] = sepBy1(p)(sep) +++ `return`(Nil.of[a])
    def sepBy1[a, sep](p: ReadP[a])(sep: ReadP[sep]): ReadP[List[a]] = liftM2(List.op_!::[a])(p)(many(sep >> p))

    def endBy[a, sep](p: ReadP[a])(sep: ReadP[sep]): ReadP[List[a]] = many { for { x <- p; _ <- sep } yield x }
    def endBy1[a, sep](p: ReadP[a])(sep: ReadP[sep]): ReadP[List[a]] = many1 { for { x <- p; _ <- sep } yield x }

    def chainr[a](p: ReadP[a])(op: ReadP[a => a => a])(x: a): ReadP[a] = chainr1(p)(op) +++ `return`(x)
    def chainl[a](p: ReadP[a])(op: ReadP[a => a => a])(x: a): ReadP[a] = chainl1(p)(op) +++ `return`(x)

    def chainr1[a](p: ReadP[a])(op: ReadP[a => a => a]): ReadP[a] = {
        lazy val scan = p >>= rest
        def rest(x: a): ReadP[a] = ( for { f <- op; y <- scan } yield f(x)(y) ) +++ `return`(x)
        scan
    }

    def chainl1[a](p: ReadP[a])(op: ReadP[a => a => a]): ReadP[a] = {
        def rest(x: a): ReadP[a] = ( for { f <- op; y <- p; * <- rest(f(x)(y)) } yield * ) +++ `return`(x)
        p >>= rest
    }

    def manyTill[a, end](p: ReadP[a])(end: ReadP[end]): ReadP[List[a]] = {
        lazy val scan: ReadP[List[a]] = (end >> `return`(Nil.of[a])) <++ liftM2(List.op_!::[a])(p)(scan)
        scan
    }

    def readP_to_S[a](p: ReadP[a]): ReadS[a] = P.run(p(P.`return`[a]))

    def readS_to_P[a](r: ReadS[a]): ReadP[a] = new ReadP[a] {
        override def apply[b](k: a => P[b]): P[b] = Look { s =>
            P.`final` {
                for {
                    (a, s_) <- r(s)
                    bs__ <- P.run(k(a))(s_)
                } yield bs__
            }
        }
    }
}
