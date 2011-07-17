

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {
/*
    def testMonadT {
        val m = MaybeT.monad[IO]
        import m.method

        def isValid(s: List[Char]): Boolean = true

        def getValidPassword: MaybeT[IO, List[Char]] = for {
            s <- MaybeT.lift(IO.getLine)
            _ <- m.guard(isValid(s))
        } yield s

        def askPassword: MaybeT[IO, Unit] = for {
            _ <- MaybeT.lift(IO.putStrLn("Insert your new password"))
            value <- getValidPassword
            _ <- MaybeT.lift(IO.putStrLn("Storing in database..."))
        } yield ()

        //MaybeT.runMaybeT(askPassword).unIO()
    }
*/

    def testMonadT2 {
        import IO.monadT.MaybeT

        def isValid(s: List[Char]): Boolean = true

        def getValidPassword: MaybeT[List[Char]] = for {
            s <- MaybeT.lift(IO.getLine)
            _ <- MaybeT.guard(isValid(s))
        } yield s

        def askPassword: MaybeT[Unit] = for {
            _ <- MaybeT.lift(IO.putStrLn("Insert your new password"))
            value <- getValidPassword
            _ <- MaybeT.lift(IO.putStrLn("Storing in database..."))
        } yield ()

        //MaybeT.run(askPassword).unIO()
    }

    def testImplicit {
        val m = Function.monad[Int]
        import m.monadT.StateT
        val mp = m.monadT.StateT.monadReader[Int, Int]
        ()
    }

    def testImplicit2 {
        val m = Function.monad[Int]
        val m_ = implicitly[Monad[m.apply]]
        import m.monadT.StateT
        val sm = implicitly[Monad[({type m[+a] = StateT[Int, a]})#m]]
        ()
    }
}
