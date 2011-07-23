

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {
    def testMonadT2 {
        import IO.MaybeT // MaybeT[a] <:< Wrap[IO[Maybe[a]]]
        import MaybeT.monad._

        def isValid(s: List[Char]): Boolean = true

        def getValidPassword: MaybeT[List[Char]] = for {
            s <- lift(IO.getLine)
            _ <- guard(isValid(s))
        } yield s

        def askPassword: MaybeT[Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- getValidPassword
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        ignore {
            askPassword.run.unIO()
        }
    }

    def testImplicit {
        val m = Function.monad[Int]
        import m.StateT
        val mp = m.StateT.monadReader[Int, Int]
        ()
    }

    def testImplicit2 {
        val m = Function.monad[Int]
        val m_ = implicitly[Monad[m.apply]]
        import m.StateT
        val sm = implicitly[Monad[({type m[+a] = StateT[Int, a]})#m]]
        ()
    }
}
