

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ListTest extends org.scalatest.junit.JUnit3Suite {

    def testZipList {
        import Applicative.{<@>, <*>}
        implicit val i = ZipList

        val xs = ((x: Int) => (y: Int) => (z: Int) => x + y + z) <@>
            List(1, 2, 3) <*> List(2, 3, 4) <*> List(3, 4, 5)

        expect(List(6, 9, 12))(xs)
    }

    def testRepeat {
        val xs = repeat(3)
        expect(List(3,3,3,3))(take(4)(xs))
    }

    def testCycle {
        val xs = cycle(List(1,2,3))
        expect(List(1,2,3,1,2,3,1,2,3,1))(take(10)(xs))
    }
}
