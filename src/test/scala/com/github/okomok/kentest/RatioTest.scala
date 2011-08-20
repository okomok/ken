

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class RatioTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val i = Integral[Kind.const[Int]]
        expect(Ratio(10, 1))(i.toRational(10))
    }

    def testGcd {
        expect(3)(Ratio.gcd(15)(6))
    }

    def testNotAmibiguous {
        val i = Ord[Kind.const[Int]]
        ()
    }
}
