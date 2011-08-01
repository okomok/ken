

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class NthTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val A1 = List(1,6,7,10,14,17)
        expect(1)(A1.!!(0))
        expect(10)(A1.!!(3))
    }
}
