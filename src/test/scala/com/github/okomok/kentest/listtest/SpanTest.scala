

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class SpanTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val t = List(3,3,3,3,3,9,7,10)
        val (fst, snd) = List.span[Int](_ == 3)(t)
        expect(List(3,3,3,3,3))(fst)
        expect(List(9,7,10))(snd)
    }

    def testEmpty: Unit = {
        val (fst, snd) = List.span[Int](_ == 3)(List.empty.of[Int])
        assert(List.`null`(fst))
        assert(List.`null`(snd))
    }

    def testBound1: Unit = {
        val t = List(3,3,3,3,9,3,3,3,3)
        val (fst, snd) = List.span[Int](_ == 4)(t)
        assert(List.`null`(fst))
        expect(List(3,3,3,3,9,3,3,3,3))(snd)
    }

    def testBound2: Unit = {
        val t = List(3,3,3,3,9,3,3,3,3)
        val (fst, snd) = List.span[Int](_ != 99)(t)
        expect(List(3,3,3,3,9,3,3,3,3))(fst)
        assert(List.`null`(snd))
    }
}
