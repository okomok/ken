

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MonadTransTest extends org.scalatest.junit.JUnit3Suite {

    def testGetInstance {
        val i = MonadIO[MaybeT.apply[IO]]
        val me = Monad[Error.apply[String]]
        val j = MonadError[String, MaybeT.apply[me.apply]]
    }
/*
    Legacy
    def testDependent {
        def testIt[n[+_]](n1: Monad[n], n2: Monad[n]) {
            val m1 = Monad[n1.MaybeT.type]
            val m2 = Monad[n2.MaybeT.type]
            import n2.MaybeT.dependent // hmm.. https://issues.scala-lang.org/browse/SI-4225
            n2.MaybeT.run(m1.`return`(Just(3)))
        }
        ()
    }

    def testDependentUp {
        def testIt[n[+_]](n1: Monad[n], n2: Monad[n]) {
            val m1 = Monad[n1.MaybeT.type]
            val m2 = Monad[n2.MaybeT.type]
            n2.MaybeT.run(m1.`return`(Just(3)).up)
        }
        ()
    }
*/
    def testImplicit {
        MonadError[String, ErrorT.apply2[String, WeakIdentity.apply]]
        MonadError[String, ErrorT.apply2[String, IO]]
        MonadIO[ListT.apply[IO]]
        MonadIO[ErrorT.apply2[String, IO]]
        MonadIO[ReaderT.apply2[String, IO]]
        MonadIO[ContT.apply2[String, IO]]
        MonadIO[StateT.apply2[Int, IO]]
        MonadIO[WriterT.apply2[Monoid.All, IO]]

        Monad[ErrorT.apply2[String, WeakIdentity.apply]]
        Monad[ErrorT.apply2[String, IO]]
        Monad[ListT.apply[IO]]
        Monad[ErrorT.apply2[String, IO]]
        Monad[ReaderT.apply2[String, IO]]
        Monad[ContT.apply2[String, IO]]
        Monad[StateT.apply2[Int, IO]]
        Monad[WriterT.apply2[Monoid.All, IO]]

        val wr = MonadWriter[Monoid.All, Writer.apply[Monoid.All]]
        MonadWriter[Monoid.All, ErrorT.apply2[String, wr.apply]]
    }

    def testControlInstance {
        val i1 = MonadTransControl[ReaderT.apply1[Int]]
        val i2 = MonadTransControl[ErrorT.apply1[String]]
        ()
    }
}
