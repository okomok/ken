

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class InferenceTest extends org.scalatest.junit.JUnit3Suite {

    def useFunctor[f[+_]](implicit i: Functor[f]) = ()

    def testWeak {
        // useFunctor(WeakIdentity) // fails to infer

        useFunctor(WeakIdentity.monad)
        useFunctor(WeakIdentity.asFunctor)
        useFunctor(Monad[WeakIdentity.apply])
        useFunctor[WeakIdentity.apply]

        val constFunctor = Const.functor[Int]
        useFunctor(constFunctor.asFunctor)
        useFunctor(Functor[constFunctor.apply])
        useFunctor[constFunctor.apply]
    }
}