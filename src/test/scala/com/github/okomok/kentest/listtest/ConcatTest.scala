

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ConcatTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t1 = List(0,1,2)
        val t2 = List(3,4)
        val t3 = List.empty.of[Int]
        val t4 = List(5,6)
        val t5 = List(7,8,9,10)
        val t = List.concat(List(t1, t2, t3, t4, t5))
        val a = List(0,1,2,3,4,5,6,7,8,9,10)
        expect(t)(a)
        expect(t)(a)
    }

    def testEmpty: Unit = {
        val t1 = List.empty.of[Int]
        val t2 = List(3,4)
        val t3 = List.empty.of[Int]
        val t4 = List.empty.of[Int]
        val t = List.concat(List(t1, t2, t3, t4))
        val a = List(3,4)
        expect(t)(a)
        expect(t)(a)
    }

    def testEmpty2: Unit = {
        val t1 = List.empty.of[Int]
        val t2 = List.empty.of[Int]
        val t3 = List.empty.of[Int]
        val t = List.concat(List(t1, t2, t3))
        assert(List.`null`(t))
        assert(List.`null`(t))
    }

    def testEmpty3: Unit = {
        val t1 = List.empty.of[Int]
        val t = List.concat(List(t1))
        assert(List.`null`(t))
        assert(List.`null`(t))
    }

    def testFlatMap: Unit = {
        val t = List(1,2,3,4,5).flatMap(e => List.`return`(e))
        val a = List(1,2,3,4,5)
        expect(t)(a)
        expect(t)(a)
    }

    def testInfinite: Unit = {
        val L = List.concat(List.repeat(List(1,2,3,4,5)))
        val A = List(1,2,3,4,5,1,2,3,4,5,1,2,3)
        expect(A)(List.take(13)(L))
    }
}
