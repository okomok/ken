

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class MatchTest extends org.scalatest.junit.JUnit3Suite {
    def testMatching: Unit = {
        val a = List(1,2,3,4,5)
        val List(x1,2,x3,4,x5) = a
        expect(1)(x1)
        expect(3)(x3)
        expect(5)(x5)

        a match {
            case List(x1,x2) => fail("doh")
            case List(x1,2,x3,4,x5) => ()
            case _ => fail("doh")
        }

        a match {
            case List(1,2,_*) => ()
            case _ => fail("doh")
        }
        ()
    }

}
