

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class RangeTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        expect(List(1,2,3,4))(List.range(1, 5))
    }
    def testEmpty: Unit = {
        assert(List.`null`(List.range(1, 1)))
    }

    def testFrom: Unit = {
        expect(List(3,4,5,6))(List.take(4)(List.rangeFrom(3)))
    }
}
