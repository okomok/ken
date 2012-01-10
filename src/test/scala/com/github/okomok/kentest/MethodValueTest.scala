

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MethodValueTest extends org.scalatest.junit.JUnit3Suite {

    type Int2Int = Int => Int

    def id1(a: Int): Int = a // Method-Types: (Int) Int
    def id2: Int2Int = a => a // Parameterless Method-Types: => Int2Int

    def plus1(a: Int)(b: Int): Int = a + b // Method-Types: (Int)(Int) Int
    def plus2(a: Int): Int2Int = b => a + b // Method-Types: (Int) Int2Int

    def testTrouble {
        val f1 = id1 _ // Method-Values: Int2Int
        val f2 = id2 _ // Method-Values: () => Int2Int
        val f3 = id2 // Evaluation. in Method-Conversions

        expect(10)(f1(10))
        expect(10)(f2()(10))
        expect(10)(f3(10))

        val g1 = plus1(10) _ // Method-Values: Int2Int
        // val g2 = plus2(10) _ // ill-formed
        val g3 = plus2(10) // Function-Applications

        expect(12)(g1(2))
        expect(12)(g3(2))
    }

    def testOk {
        val f1 = Function.from(id1)
        val f2 = Function.from(id2)

        expect(10)(f1(10))
        expect(10)(f2(10))

        val g1 = Function.from(plus1(10))
        val g2 = Function.from(plus2(10))

        expect(12)(g1(2))
        expect(12)(g2(2))
    }
}
