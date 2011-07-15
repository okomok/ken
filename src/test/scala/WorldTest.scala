

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class WorldTest extends org.scalatest.junit.JUnit3Suite {

    def sumST[a](xs: List[a])(implicit i: Num[a]): a = {
        val w = new World
        import w._
        import w.ST.monad._
        import i._
        runST {
            for {
                n <- newSTRef(fromInt(0))
                _ <- forM_(xs) { x =>
                    for {
                        r <- modifySTRef(n)(x + _)
                    } yield r
                }
                r <- readSTRef(n)
            } yield r
        }
    }

    def testTrival {
        expect(15)(sumST(List(1,2,3,4,5)))
    }
}
