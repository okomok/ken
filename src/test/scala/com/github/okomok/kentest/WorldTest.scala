

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class WorldTest extends org.scalatest.junit.JUnit3Suite {

    def sumST[a](xs: List[a])(implicit i: Num[a]): a = {
        val w = new World
        import w._
        import i._
        import ST.monad.{`for`, forM_}
        runST {
            for {
                n <- newSTRef(fromInteger(0))
                _ <- forM_(xs) { x =>
                    for {
                        * <- modifySTRef(n)(x + _)
                    } yield *
                }
                * <- readSTRef(n)
            } yield *
        }
    }

    def testTrival {
        expect(15)(sumST(List(1,2,3,4,5)))
    }

    def testMonad {
        val w = new World
        implicitly[Monad[w.ST]]
    }
}
