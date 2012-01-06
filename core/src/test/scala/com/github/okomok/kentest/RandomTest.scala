

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class RandomTest extends org.scalatest.junit.JUnit3Suite {

    def printFor[g, a](a: Type[a])(g: g)(ival: (a, a))(c: Int)(implicit i: RandomGen[g], j: Random[a]) {
        if (c != 0) {
            val xg = j.randomR(ival)(g)
            println(xg._1)
            printFor(a)(g)(ival)(c - 1)
        }
    }

    def testInt {
        def assertInRange[g](g: g)(ival: (Int, Int))(c: Int)(implicit i: RandomGen[g]) {
            if (c != 0) {
                val xg = Int.randomR(ival)(g)
                assert(ival._1 <= xg._1 && xg._1 <= ival._2)
                assertInRange(xg._2)(ival)(c - 1)
            }
        }
        assertInRange(StdGen(789))(100, 200)(100)
        assertInRange(StdGen(789))(-100, -30)(100)
        assertInRange(StdGen(789))(-100, 30)(100)
        assertInRange(StdGen(789))(0, 1)(100)
        assertInRange(StdGen(789))(0, 0)(100)
    }

    def testTrivial {
        //printFor(Type[Bool])(StdGen(981))(True, True)(100)
        //printFor(Type[Bool])(StdGen(981))(False, False)(100)
        //printFor(Type[Bool])(StdGen(981))(False, True)(100)
    }

    def testDouble {
        def assertInRange[g](g: g)(ival: (Double, Double))(c: Double)(implicit i: RandomGen[g]) {
            if (c != 0) {
                val xg = Double.randomR(ival)(g)
                assert(ival._1 <= xg._1 && xg._1 <= ival._2)
                assertInRange(xg._2)(ival)(c - 1)
            }
        }
        assertInRange(StdGen(789))(100, 200)(100)
        assertInRange(StdGen(789))(-100, -30)(100)
        assertInRange(StdGen(789))(-100, 30)(100)
        assertInRange(StdGen(789))(0, 1)(100)
        assertInRange(StdGen(789))(0, 0)(100)
    }

    def testFloat {
        def assertInRange[g](g: g)(ival: (Float, Float))(c: Float)(implicit i: RandomGen[g]) {
            if (c != 0) {
                val xg = Float.randomR(ival)(g)
                assert(ival._1 <= xg._1 && xg._1 <= ival._2)
                assertInRange(xg._2)(ival)(c - 1)
            }
        }
        assertInRange(StdGen(789))(100, 200)(100)
        assertInRange(StdGen(789))(-100, -30)(100)
        assertInRange(StdGen(789))(-100, 30)(100)
        assertInRange(StdGen(789))(0, 1)(100)
        assertInRange(StdGen(789))(0, 0)(100)
    }

}
