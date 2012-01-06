

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class MapTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(1,2,3)
        val u = List(2,3,4)
        val k = List.map[Int, Int](_ + 1)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testEmpty: Unit = {
        val t = List.empty.of[Int]
        val u = List.empty.of[Int]
        val k = List.map[Int, Int](_ + 1)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testMapMap: Unit = {
        val t = List(1,2,3)
        val u = List(7,8,9)
        val k = List.map[Int, Int](_ + 3)(List.map[Int, Int](_ + 2)(List.map[Int, Int](_ + 1)(t)))

        expect(u)(k)
        expect(u)(k)
    }
}
