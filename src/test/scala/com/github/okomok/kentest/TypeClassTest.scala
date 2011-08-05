

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
        val m = Monad[Function.apply[Int]]
        ()
    }
}
