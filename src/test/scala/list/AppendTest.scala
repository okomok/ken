

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class AppendTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial: Unit = {
        val t = List(4,5,1,3)
        val u = List(9,7,10)
        val v = List(4,5,1,3,9,7,10)
        val k = t ::: u
        expect(v)(k)
        expect(v)(k)
    }

    def testEmpty: Unit = {
        val k = List.empty.of[Int] ::: List.empty.of[Int]
        expect(List.empty.of[Int])(k)
        expect(List.empty.of[Int])(k)
    }

    def testEmpty2: Unit = {
        val t = List(4,5,1,3)
        val t_  = List(4,5,1,3)
        val k = List.empty.of[Int] ::: t
        expect(t_)(k)
        expect(t_)(k)
    }

    def testEmpty3: Unit = {
        val t = List(4,5,1,3)
        val t_  = List(4,5,1,3)
        val k = t ::: List.empty.of[Int]
        expect(t_)(k)
        expect(t_)(k)
    }

    def testNonTrivial: Unit = {
        val t1 = List(4,5)
        val t2 = List(1)
        val t3 = List(3, 9)
        val t4 = List.empty.of[Int]
        val t5 = List(7,10,11)
        val v = List(4,5,1,3,9,7,10,11)
        val k = t1 ::: t2 ::: t3 ::: t4 ::: t5
        expect(v)(k)
        expect(v)(k)
    }
/*
    def testReverse: Unit = {
        val t = List(4,5,1,3)
        val u = List(9,7,10)
        val v = List(3,1,5,4,9,7,10)
        val k = t reverse_::: u
        expect(v)(k)
        expect(v)(k)
    }
*/
}
