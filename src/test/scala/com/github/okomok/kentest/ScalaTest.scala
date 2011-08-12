

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ScalaTest extends org.scalatest.junit.JUnit3Suite {

    def testList {
        val m = implicitly[Monad[scala.List]]
        import m._
        val ret =  scala.List(1,2,3) >>= { x => scala.List(x, x+10) }
        expect(scala.List(1,11,2,12,3,13))(ret)
    }

    def testListOne {
        val m = Monad[Kind.quote1[scala.List]]
        val ret = m.`return`(12)
        expect(scala.List(12))(ret)
    }

    def testNil {
        val m = MonadPlus[Kind.quote1[scala.List]]
        val ret: scala.List[Nothing] = m.mzero
        expect(scala.Nil)(ret)
    }

    def testOption {
        val m = MonadPlus[Scala.Option.type]
        val ret: scala.Option[Nothing] = m.mzero
        expect(scala.None)(ret)
    }

    def testNoAmbiguity {
        val f = implicitly[Functor[scala.List]]
        val g = implicitly[Foldable[scala.List]]
    }
}
