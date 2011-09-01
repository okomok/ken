

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsectest


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    import parsec.Parsers._

    def testTrivial {
        val p: Parser[Char] = char('a')
        expect(Right('a')) {
            parse(p)("trivial")(List.from("a"))
        }

        parse(p)("trivial")(Nil.of[Char]) match {
            case Left(_) => ()
            case Right(_) => fail("doh")
        }

        expect(Right(List.from("aaaaa"))) {
            parse(many(p))("trivial")(List.from("aaaaabb"))
        }

    }

    def test_ {}
}
