

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class TakeTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(4,5,1,3,2,9,7,10)
        val u = List(4,5,1)
        val v = List(4,5,1,3,2,9,7,10)
        val k0 = List.take(0)(t)
        assert(List.`null`(k0))
        assert(List.`null`(k0))
        val k1 = List.take(3)(t)
        expect(u)(k1)
        expect(u)(k1)
        val k2 = List.take(8)(t)
        expect(v)(k2)
        expect(v)(k2)
        val k3 = List.take(80)(t)
        expect(v)(k3)
        expect(v)(k3)
    }

    def testTrivial2: Unit = {
        val t = List(14,15,11,3,2,9,7,10)
        val u = List(14,15,11)
        val v = List(14,15,11,3,2,9,7,10)
        val k0 = List.takeWhile[Int](_ > 100)(t)
        assert(List.`null`(k0))
        assert(List.`null`(k0))
        val k1 = List.takeWhile[Int](_ > 10)(t)
        expect(u)(k1)
        expect(u)(k1)
        val k2 = List.takeWhile[Int](_ => true)(t)
        expect(v)(k2)
        expect(v)(k2)
    }

    def testTakeTake: Unit = {
        val t = List(9,7,10,1,2)
        val u = List(9,7)
        val k = List.take(3)(List.take(2)(t))
        expect(k)(u)
    }

    def testEmpty: Unit = {
        val k = List.take(2)(List.empty.of[Int])
        assert(List.`null`(k))
        assert(List.`null`(k))
        val k2 = List.take(0)(List.empty.of[Int])
        assert(List.`null`(k2))
        assert(List.`null`(k2))
    }
}
