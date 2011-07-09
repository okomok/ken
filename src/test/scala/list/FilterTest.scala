

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class FilterTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(1,2,2,3,4,5,5,6,7,8,9)
        val u = List(2,2,4,6,8)
        val k = List.filter[Int](_ % 2 == 0)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testTrivial2: Unit = {
        val t = List(2,2,3,4,5,5,6,7,8,9)
        val u = List(2,2,4,6,8)
        val k = List.filter[Int](_ % 2 == 0)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testEmpty: Unit = {
        val t = List.empty.of[Int]
        val k = List.filter[Int](_ % 2 == 0)(t)
        assert(List.`null`(k))
        assert(List.`null`(k))
    }

    def testEmpty2: Unit = {
        val t = List(1,2,2,3,4,5,5,6,7,8,9)
        val k = List.filter[Int](_ % 20 == 0)(t)
        assert(List.`null`(k))
        assert(List.`null`(k))
    }

    def testFilterFilter: Unit = {
        val t = List(1,2,2,3,4,5,5,6,7,8,9)
        val u = List(4,8)
        val k = List.filter[Int](_ % 2 == 0)(List.filter[Int](_ % 4 == 0)(t))
        expect(u)(k)
        expect(u)(k)
    }
}
