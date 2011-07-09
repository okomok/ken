

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class DropTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(4,5,1,3,2,9,7,10)
        val u = List(9,7,10)
        val v = List(10)
        val k = List.drop(5)(t)
        expect(u)(k)
        expect(u)(k)
        val k_ = List.drop(7)(t)
        expect(v)(k_)
        expect(v)(k_)
        assert(List.`null`{List.drop(8)(t)})
        assert(List.`null`{List.drop(9)(t)})
        assert(List.`null`{List.drop(80)(t)})
    }

    def testWhile: Unit = {
        val t = List(3,3,3,3,3,9,7,10)
        val u = List(9,7,10)
        val v = List(10)
        val k = List.dropWhile[Int](_ == 3)(t)
        expect(u)(k)
        assert(List.`null`{List.dropWhile[Int](_ != 99)(t)})
        expect(t)(List.dropWhile[Int](_ => false)(t))
    }

    def testDropDrop: Unit = {
        val t = List(3,3,3,3,3,9,7,10)
        val u = List(9,7,10)
        val k = List.drop(3)(List.drop(2)(t))
        expect(k)(u)
    }

    def testEmpty: Unit = {
        val k = List.drop(3)(List.empty.of[Int])
        assert(List.`null`(k))
        assert(List.`null`(k))
    }
}
