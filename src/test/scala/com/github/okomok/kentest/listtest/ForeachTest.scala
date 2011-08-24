

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ForeachTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val tr = List(1,2,3)
        val k1 = new java.util.ArrayList[Int]
        val k2 = new java.util.ArrayList[Int]
        tr.foreach(e => k1.add(e))
        k2.add(1); k2.add(2); k2.add(3)
        expect(k1)(k2)
    }

    def testEmpty: Unit = {
        val tr = List.empty.of[Int]
        val k1 = new java.util.ArrayList[Int]
        val k2 = new java.util.ArrayList[Int]
        tr.foreach(e => k1.add(e))
        expect(k1)(k2)
    }
}
