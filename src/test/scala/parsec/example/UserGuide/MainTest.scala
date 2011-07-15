

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package parsectest.example.userguide


import com.github.okomok.ken._
import Parsec._


class MainTezt /*extends org.scalatest.junit.JUnit3Suite*/ {

    // 2.1

    val simple: Parser[Char] = letter

    def run[a](p: Parser[a])(input: String_): IO[Unit] = {
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
        run(simple)("a").unIO()
        run(simple)("").unIO()
        run(simple)("123").unIO()
    }

    // 2.2

    val openClose: Parser[Char] = for {
        _ <- char('(')
        c <- char(')')
    } yield c

    val parens: Parser[Unit] = {
        val i = Parser.monad

        ( for {
            _ <- char('(')
            _ <- parens
            _ <- char(')')
            _ <- parens
        } yield () ) <|> i.`return`(())
    }

    def testParens {
        println("---run parens---")
        run(parens)("(())()").unIO()
        run(parens)("(()()").unIO()
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
            _ <- char('(')
            _ <- char('a') <|> char('b')
            _ <- char(')')
        } yield ()
    }

    val testOr2 = `try`(string[Unit]("(a)")) <|> string[Unit]("(b)")

    def testTestOr {
        println("---run testOr---")
        run(testOr2)("(b)").unIO()
    }

    // 2.4

    lazy val nesting: Parser[Int] = {
        val i = Parser.monad
        val j = Ord[Int]
        ( for {
            _ <- char('(')
            n <- nesting
            _ <- char(')')
            m <- nesting
        } yield (j.max(n+1)(m)) ) <|> i.`return`(0)
    }

    def testNesting {
        println("---run nesting---")
        run(nesting)("(())()").unIO()
        run(nesting)("(()(()))").unIO()
        run(nesting)("(()(())").unIO()
    }

    // 2.5 and 2.6

    lazy val _word: Parser[String_] = for {
        c <- letter
        d <- ( for { cs <- _word } yield  c :: cs ) <|> Parser.monad.`return`(List(c))
    } yield d

    val word: Parser[String_] = many1(letter[Unit] <#> "") <#> "word"

    def testWord {
        println("---run word---")
        run(word)("hidi,,").unIO()
    }

    lazy val sentence: Parser[List[String_]] = for {
        words <- sepBy1(word)(separator)
        _ <- oneOf(".?!") <#> "end of sentence"
    } yield words

    lazy val separator: Parser[Unit] = skipMany1(space <|> char(',') <#> "")

    def testSentence {
        println("---run sentence---")
        run(sentence)("hi,di,hi.").unIO()
        run(sentence)("hi,di hi!").unIO()
        run(sentence)("hi,123").unIO()

        run(sentence)("hi di").unIO()
        run(sentence)("hi di,").unIO()
    }

    // 2.7
}
