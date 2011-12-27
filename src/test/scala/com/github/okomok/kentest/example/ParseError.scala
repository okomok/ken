

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// From: http://monads.haskell.cz/html/errormonad.html


class ParseErrorTest extends org.scalatest.junit.JUnit3Suite {

    final class ParseError(val location: Int, val reason: String)

    object Err {
        def apply(l: Int)(r: String): ParseError = new ParseError(l, r)
    }

    implicit object ParseErrorClass extends ErrorClass[ParseError] {
        override def noMsg = Err(0)("ParseError")
        override def strMsg = s => Err(0)(s)
    }

    implicit val ParseMonad = MonadError[Error.apply[ParseError]]
    type ParseMonad[+a] = ParseMonad.apply[a]

    import ParseMonad._

    def parseHexDigit: Char => Int => ParseMonad[Int] = c => idx => {
        if (Char.isHexDigit(c)) {
            `return`(Char.digitToInt(c))
        } else {
            throwError(Err(idx)("Invalid character " ++: List(c)))
        }
    }

    def parseHex: String => ParseMonad[Int] = { s =>
        def parseHex_ : String => Int => Int => ParseMonad[Int] = cs => v => idx => cs match {
            case Nil =>`return`(v)
            case c :: cs => for {
                d <- parseHexDigit(c)(idx)
            } {
                parseHex_(cs.!)(v * 16 + d)(idx + 1)
            }
        }
        parseHex_(s)(0)(1)
    }

    def toString_ : Int => ParseMonad[String] = n => `return` { Show.show(n) }

    def convert: String => String = s => {
        val printError: ParseError => ParseMonad[String] = e => {
            `return` { "At Index " ++: Show.show(e.location) ++: ": " ++: e.reason }
        }

        val Right(str) = catchError( for { n <- parseHex(s) } { toString_(n) } )(printError).run
        str
    }

    def testTrivial {
        expect(List.from("65385"))(convert("ff69"))
        expect(List.from("At Index 4: Invalid character z"))(convert("ff6z"))
    }
}
