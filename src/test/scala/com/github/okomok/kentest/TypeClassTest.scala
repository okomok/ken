

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TypeClassTest extends org.scalatest.junit.JUnit3Suite {

    def testImplicitObsolete {
        val m = Function.monad[Int]
        import m.StateT
        val mp = m.StateT.asMonadReader[Int, Int]
        ()
    }

    def testImplicit2Obsolete {
        val m = Function.monad[Int]
        val m_ = implicitly[Monad[m.apply]]
        import m.StateT
        val sm = implicitly[Monad[({type m[+a] = StateT[Int, a]})#m]]
        ()
    }

    def testImplicit {
        val fm = Monad[Function.apply[Int]]
        val im = Monad[Identity.type]
        val lm = Monad[List.type]
        val wim = Monad[WeakIdentity.type]
        val mit = Monad[Identity.ListT.type]

        def infer[m[+_]](m: Monad[m]) = ()

        infer(fm)
        infer(im)
        infer(lm)
        infer(wim)
        infer(mit)
    }

    def testMonadTrans {
        val mt1 = MonadTrans[IO.StateT.apply[Int]]
        val mt2 = MonadTrans[IO.LazyT.type]
    }

    def testWeak1 {
        val wt1 = Weak1[IO.LazyT.type]
    }
}
