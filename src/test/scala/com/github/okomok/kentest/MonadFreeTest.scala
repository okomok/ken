

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MonadFreeTest extends org.scalatest.junit.JUnit3Suite {

    def testFreeT {
        FreeT._asMonadFree[Identity, Identity]
        Functor[Function.apply[Int]]
        MonadFree[Identity.type, FreeT.apply2[Identity, Identity]]
        Functor[FreeT.apply2[Identity, Identity]]
//        MonadFree[Function.apply[Int], FreeT.apply2[Identity, Identity]] // `f` isn't binded, so that lookup fails.
    }

    def testFree {
        MonadFree[Identity.type, Free.apply[Identity]]
        Functor[Free.apply[Identity]]
    }

    def testCodensity {
        Monad[Codensity.apply[Identity]]
        Functor[Codensity.apply[Identity]]
    }
}
