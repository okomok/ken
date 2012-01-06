

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsectest.example.userguide


import com.github.okomok.ken._
import parsec._


class MainTezt {
//class MainTest extends org.scalatest.junit.JUnit3Suite {

    // 2.1

    val pop = ParsecTOp[Parser[Char]]
    import pop._

    val simple: Parser[Char] = letter
    val pm = MonadPlus[Parser.type]
    import pm._

    def run[a](p: Parser[a])(input: String)(implicit i: Show[a]): IO[Unit] = {
        val io = parse(p)("")(input) match {
            case Left(err) => for {
                _ <- IO.putStr("parse error at ")
                _ <- IO.print(err)
            } yield ()
            case Right(x) => IO.print(x)
        }

        for {
            _ <- io
            _ <- IO.print("\n\n")
        } yield ()
    }

    def testSimple {
        println("---run simple---")
        run(simple)("a").!
        run(simple)("").!
        run(simple)("123").!
    }

    // 2.2

    val openClose: Parser[Char] = for {
        _ <- char('(')
        c <- char(')')
    } yield c

    val parens: Parser[Unit] = {
        ( for {
            _ <- char('(')
            _ <- parens
            _ <- char(')')
            _ <- parens
        } yield () ) <|> `return`()
    }

    def testParens {
        println("---run parens---")
        run(parens)("(())()").!
        run(parens)("(()()").!
    }

    // 2.3

    val testOr = string("(a)") <|> string("(b)")

    val _testOr: Parser[Unit] = {
        (for { _ <- char('('); _ <- char('a'); _ <- char(')') } yield () ) <|>
        (for { _ <- char('('); _ <- char('a'); _ <- char(')') } yield () )
    }

    val testOr1: Parser[Unit] = {
        for {
            _ <- char('(')
            _ <- char('a') <|> char('b')
            _ <- char(')')
        } yield ()
    }

    val testOr2 = `try`(string("(a)")) <|> string("(b)")

    def testTestOr {
        println("---run testOr---")
        run(testOr2)("(b)").!
    }

    // 2.4

    lazy val nesting: Parser[Int] = {
        val j = instance[Ord[Int]]
        ( for {
            _ <- char('(')
            n <- nesting
            _ <- char(')')
            m <- nesting
        } yield (j.max(n+1)(m)) ) <|> `return`(0)
    }

    def testNesting {
        println("---run nesting---")
        run(nesting)("(())()").!
        run(nesting)("(()(()))").!
        run(nesting)("(()(())").!
    }

    // 2.5 and 2.6

    lazy val _word: Parser[String] = for {
        c <- letter
        d <- ( for { cs <- _word } yield  c :: cs ) <|> `return`(List(c))
    } yield d

    val word: Parser[String] = many1(letter <#> "") <#> "word"

    def testWord {
        println("---run word---")
        run(word)("hidi,,").!
    }

    lazy val sentence: Parser[List[String]] = for {
        words <- sepBy1(word)(separator)
        _ <- oneOf(".?!") <#> "end of sentence"
    } yield words

    lazy val separator: Parser[Unit] = skipMany1(space <|> char(',') <#> "")

    def testSentence {
        println("---run sentence---")
        run(sentence)("hi,di,hi.").!
        run(sentence)("hi,di hi!").!
        run(sentence)("hi,123").!

        run(sentence)("hi di").!
        run(sentence)("hi di,").!
    }

    // 2.7
}
