

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MonadTransTest extends org.scalatest.junit.JUnit3Suite {

    def testGetInstance {
        val i = MonadIO[IO.MaybeT.type]
        val me = Monad[Error.apply[String_]]
        val j = MonadError[String_, me.MaybeT.type]
    }

    def testDependent {
        def testIt[n[+_]](n1: Monad[n], n2: Monad[n]) {
            val m1 = Monad[n1.MaybeT.type]
            val m2 = Monad[n2.MaybeT.type]
            import n2.MaybeT.dependent // hmm.. https://issues.scala-lang.org/browse/SI-4225
            n2.MaybeT.run(m1.`return`(Just(3)))
        }
        ()
    }
}
