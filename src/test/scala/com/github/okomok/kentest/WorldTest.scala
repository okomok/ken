

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class WorldTest extends org.scalatest.junit.JUnit3Suite {

    def sumST[a](xs: List[a])(implicit i: Num[a]): a = {
        val w = new World
        import w._
        val sm = Monad[ST.type]
        import sm.{`for`, forM_}
        runST {
            for {
                n <- newSTRef(i.fromInteger(0))
                _ <- forM_(xs) { x =>
                    for {
                        * <- modifySTRef(n)(i.op_+(x))
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

    def testIncompatible {
        val w1 = new World
        val w2 = new World
        def makeSTRef2: w2.STRef[Int] = error("dummy")
        //w1.readSTRef(makeSTRef2) // never compiles.
    }
}
