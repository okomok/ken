

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class UnfoldTest extends org.scalatest.junit.JUnit3Suite {
    def testRight: Unit = {
        val E1 = List.unfoldr((b: Int) => if (b == 0) Nothing else Just(b, b-1))(10)
        expect(List(10,9,8,7,6,5,4,3,2,1))(E1)
    }

    def testIterate: Unit = {
        val E1 = List.iterate(2 * (_: Int))(1)
        expect(List(1,2,4,8,16,32,64,128,256,512))(List.take(10)(E1))
    }

    def testRepeat: Unit = {
        val E1 = List.repeat(3)
        expect(List(3,3,3,3,3,3,3,3,3,3))(List.take(10)(E1))
    }

    def testReplicate: Unit = {
        val E1 = List.replicate(10)(3)
        expect(List(3,3,3,3,3,3,3,3,3,3))(E1)
    }
}
