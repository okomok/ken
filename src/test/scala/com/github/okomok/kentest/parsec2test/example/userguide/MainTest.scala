

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsec2test.example.userguide


import com.github.okomok.ken._
import Parsec2._


class MainTezt {
//class MainTest extends org.scalatest.junit.JUnit3Suite {

    // 2.1

    val simple: Parser[Char] = letter
    val pm = MonadPlus[Parser.type]
    import pm._
    val char_ = char[Unit]_

    def run[a](p: Parser[a])(input: String_)(implicit i: Show[a]): IO[Unit] = {
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
        _ <- char_('(')
        c <- char_(')')
    } yield c

    val parens: Parser[Unit] = {
        ( for {
            _ <- char_('(')
            _ <- parens
            _ <- char_(')')
            _ <- parens
        } yield () ) <|> `return`()
    }

    def testParens {
        println("---run parens---")
        run(parens)("(())()").!
        run(parens)("(()()").!
    }

    // 2.3

    val testOr = string[Unit]("(a)") <|> string[Unit]("(b)")

    val _testOr: Parser[Unit] = {
        val char_ = char[Unit] _
        (for { _ <- char_('('); _ <- char_('a'); _ <- char_(')') } yield () ) <|>
        (for { _ <- char_('('); _ <- char_('a'); _ <- char_(')') } yield () )
    }

    val testOr1: Parser[Unit] = {
        for {
            _ <- char_('(')
            _ <- char_('a') <|> char_('b')
            _ <- char_(')')
        } yield ()
    }

    val testOr2 = `try`(string[Unit]("(a)")) <|> string[Unit]("(b)")

    def testTestOr {
        println("---run testOr---")
        run(testOr2)("(b)").!
    }

    // 2.4

    lazy val nesting: Parser[Int] = {
        val j = implicitly[Ord[Int]]
        ( for {
            _ <- char_('(')
            n <- nesting
            _ <- char_(')')
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

    lazy val _word: Parser[String_] = for {
        c <- letter[Unit]
        d <- ( for { cs <- _word } yield  c :: cs ) <|> `return`(List(c))
    } yield d

    val word: Parser[String_] = many1(letter[Unit] <#> "") <#> "word"

    def testWord {
        println("---run word---")
        run(word)("hidi,,").!
    }

    lazy val sentence: Parser[List[String_]] = for {
        words <- sepBy1(word)(separator)
        _ <- oneOf[Unit](".?!") <#> "end of sentence"
    } yield words

    lazy val separator: Parser[Unit] = skipMany1(space[Unit] <|> char_(',') <#> "")

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
