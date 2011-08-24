

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package parsectest


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def teztTrivial: Unit = {
        val p = Parsec.many(Parsec.char[Unit]('a'))
        println {
            Parsec.parse(p)("trivial")(List.from("aaaba"))
        }
        println {
            Parsec.runP(p)(Parsec.State(List.from("aaabb"), Parsec.initialPos("trivial"), ()))
        }
    }

    def test_ {}
}
