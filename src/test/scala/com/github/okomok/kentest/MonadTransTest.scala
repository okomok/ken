

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MonadTransTest extends org.scalatest.junit.JUnit3Suite {

    def testGetInstance {
        val i = MonadIO[MaybeT.apply[IO.type]]
        val me = Monad[Error.apply[String]]
        val j = MonadError[MaybeT.apply[me.type]]
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
        MonadError[ErrorT.apply2[String, WeakIdentity.type]]
        MonadError[ErrorT.apply2[String, IO.type]]
        MonadIO[ListT.apply[IO.type]]
        MonadIO[ErrorT.apply2[String, IO.type]]
        MonadIO[ReaderT.apply2[String, IO.type]]
        MonadIO[ContT.apply2[String, IO.type]]
        MonadIO[StateT.apply2[Int, IO.type]]
        MonadIO[WriterT.apply2[Monoid.All, IO.type]]

        Monad[ErrorT.apply2[String, WeakIdentity.type]]
        Monad[ErrorT.apply2[String, IO.type]]
        Monad[ListT.apply[IO.type]]
        Monad[ErrorT.apply2[String, IO.type]]
        Monad[ReaderT.apply2[String, IO.type]]
        Monad[ContT.apply2[String, IO.type]]
        Monad[StateT.apply2[Int, IO.type]]
        Monad[WriterT.apply2[Monoid.All, IO.type]]

        val wr = MonadWriter[Writer.apply[Monoid.All]]
        MonadWriter[ErrorT.apply2[String, wr.type]]
    }

    def testControlInstance {
        val i1 = MonadTransControl[ReaderT.apply1[Int]]
        val i2 = MonadTransControl[ErrorT.apply1[String]]
        ()
    }
}
