

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsectest


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    import parsec.Parsec

    def testTrivial {
        val p: parsec.Parser[Char] = Parsec.char[List[Char], Unit]('a')
        expect(Right('a')) {
            Parsec.parse(p)("trivial")(List.from("a"))
        }

        Parsec.parse(p)("trivial")(Nil.of[Char]) match {
            case Left(_) => ()
            case Right(_) => fail("doh")
        }

        expect(Right(List.from("aaaaa"))) {
            Parsec.parse(Parsec.many(p))("trivial")(List.from("aaaaabb"))
        }

    }

    def test_ {}
}
