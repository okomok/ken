

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package parsectest.example.userguide


import com.github.okomok.ken._
import Parsec._


class MainTest extends org.scalatest.junit.JUnit3Suite {

    val simple: Parser[Char] = letter

    def run[a](p: Parser[a])(input: List[Char]): IO[Unit] = {
        parse(p)("")(input) match {
            case Left(err) => for {
                _ <- IO.putStr("parse error at ")
                _ <- IO.printLn(err)
            } yield ()
            case Right(x) => IO.printLn(x)
        }
    }

    def testTrivial {
        run(simple)("a").unIO()
        run(simple)("").unIO()
        run(simple)("123").unIO()
    }

}
