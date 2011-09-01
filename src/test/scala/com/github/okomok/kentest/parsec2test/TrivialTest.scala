

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsec2test


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def teztTrivial: Unit = {
        val p = Parsec2.many(Parsec2.char[Unit]('a'))
        println {
            Parsec2.parse(p)("trivial")(List.from("aaaba"))
        }
        println {
            Parsec2.runP(p)(Parsec2.State(List.from("aaabb"), Parsec2.initialPos("trivial"), ()))
        }
    }

    def test_ {}
}
