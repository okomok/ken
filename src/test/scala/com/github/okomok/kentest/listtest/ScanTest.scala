

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ScanTest extends org.scalatest.junit.JUnit3Suite {
// left
    def testScanLeft: Unit = {
        val t = List(1,2,3,4,5,6,7,8)
        val u = List(5,6,8,11,15,20,26,33,41)
        val k = List.scanl[Int, Int](x => y => x + y)(5)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testScanLeftX: Unit = {
        val t = List(1)
        val u = List(5,6)
        val k = List.scanl[Int, Int](x => y => x + y)(5)(t)
        expect(u)(k)
        expect(u)(k)
    }

    def testEmpty: Unit = {
        expect(List(0))(List.scanl[Int, Int](x => y => x + y)(0)(List.empty))
    }

    def testScanLeft1: Unit = {
        val t = List(5,1,2,3,4,5,6,7,8)
        val u = List(5,6,8,11,15,20,26,33,41)
        val k = List.scanl1[Int](x => y => x + y)(t)
        expect(u)(k)
        expect(u)(k)
    }

// right
    def testScanRight: Unit = {
        val L = List(1,2,3,4)
        val A = List(15,14,12,9,5)
        expect(A)(List.scanr[Int, Int](x => y => x + y)(5)(L))
    }

    def testScanRight1: Unit = {
        val L = List(1,2,3,4)
        val A = List(10,9,7,4)
        expect(A)(List.scanr1[Int](x => y => x + y)(L))
    }

    def testScanRightInfinite: Unit = {
        val L = List.cycle(List(false,false,true,false))
        val A = List.repeat(true)
        val L_ = List.take(50)(List.scanr[Boolean, Boolean](x => y => x || y)(false)(L))
        expect(List.take(50)(A))(L_)
        ()
    }

    def testScanRight1Infinite: Unit = {
        val L = List.cycle(List(false,false,true,false))
        val A = List.repeat(true)
        expect(List.take(50)(A))(List.take(50)(List.scanr1[Boolean](x => y => x || y)(L)))
    }

}
