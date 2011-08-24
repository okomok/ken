

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class RatioTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val i = Integral[Kind.const[Int]]
        expect(Ratio(10, 1))(i.toRational(10))
    }

    def testTrivial2 {
        expect(Ratio(15, 6))(Ratio(5, 2))
    }

    def testNotAmibiguous {
        val i = Ord[Kind.const[Int]]
        ()
    }

    def testMatch {
        val Ratio(n, d) = Ratio(18, 9)
        expect((2, 1))((n, d))
    }
}
