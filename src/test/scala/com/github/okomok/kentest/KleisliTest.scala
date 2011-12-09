

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class KleisliTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        Arrow[Kleisli.apply[WeakIdentity.type]] // not ambiguous

        val a = ArrowApply[Kleisli.apply[WeakIdentity.type]]
        Monad[ArrowMonad.apply[a.apply2]]
    }
}
