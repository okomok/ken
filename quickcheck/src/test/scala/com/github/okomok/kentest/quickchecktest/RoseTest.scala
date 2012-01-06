

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.quickchecktest


import com.github.okomok.ken._
import quickcheck._


class RoseTest extends org.scalatest.junit.JUnit3Suite {

    def testJoin {
        val r1: Rose[Int] = Rose(10, List(Rose(9, Nil), Rose(8, Nil)))
        val r2: Rose[Int] = Rose(7, Nil)

        val rr: Rose[Rose[Int]] = Rose(r2, List(Rose(r1, Nil)))
        val jo: Rose[Int] = Rose.join(rr)
        expect(7)(jo.x)

        val Rose(x1, !(Rose(x2, _) :: xs)) = jo: Rose[Int]

        expect(7)(x1)
        expect(10)(x2)
    }
}
