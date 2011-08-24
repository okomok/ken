

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class SliceTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(4,5,1,3,2,9,7,10)
        val u = List(4,5,1)
        val v = List(4,5,1,3,2,9,7,10)
        val w = List(1,3,2,9)
        val x = List(1,3,2,9,7,10)
        val k0 = List.slice(3, 3)(t)
        assert(List.`null`(k0))
        assert(List.`null`(k0))
        val k00 = List.slice(0, 0)(t)
        assert(List.`null`(k00))
        assert(List.`null`(k00))
        val k1 = List.slice(0, 3)(t)
        expect(u)(k1)
        expect(u)(k1)
        val k2 = List.slice(0, 8)(t)
        expect(v)(k2)
        expect(v)(k2)
        val k3 = List.slice(0, 80)(t)
        expect(v)(k3)
        expect(v)(k3)
        val k4 = List.slice(2, 6)(t)
        expect(w)(k4)
        expect(w)(k4)
        val k5 = List.slice(2, 60)(t)
        expect(x)(k5)
        expect(x)(k5)
    }

    def testEmpty: Unit = {
        val k = List.slice(4, 4)(List.empty.of[Int])
        assert(List.`null`(k))
        val k0 = List.slice(0, 0)(List.empty.of[Int])
        assert(List.`null`(k0))
    }
}
