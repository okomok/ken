

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class LengthTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val tr = List(1,2,3)
        expect(3)(List.length(tr))
        expect(3)(List.length(tr))
    }

    def testEmpty: Unit = {
        val tr = List.empty.of[Int]
        expect(0)(List.length(tr))
        expect(0)(List.length(tr))
    }
}
