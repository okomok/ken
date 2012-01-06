

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class FindTest extends org.scalatest.junit.JUnit3Suite {
    def testFind: Unit = {
        val tr = List(2,4,6)
        expect(Just(4))(List.find[Int](_ == 4)(tr))
        expect(Nothing)(List.find[Int](_ == 9)(tr))
    }

    def testContains: Unit = {
        val tr = List(2,4,6)
        assert(List.elem(4)(tr))
    }

    def testForall: Unit = {
        val tr = List(2,4,6)
        assert(List.all[Int](_ % 2 == 0)(tr))
    }

    def testExists: Unit = {
        val tr = List(2,4,6)
        assert(List.any[Int](_ == 6)(tr))
    }
/*
    def testCount: Unit = {
        val tr = List(2,7,4,1,6)
        expect(3)(tr.count(_ % 2 == 0))
    }
*/
}
