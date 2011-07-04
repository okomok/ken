

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {

    def testMonadT {

        import MaybeT.monadTrans
        import MonadTrans.lift

        def isValid(s: List[Char]): Boolean = true

        def getValidPassword: MaybeT[IO, List[Char]] = for {
            s <- lift(IO.getLine)
            _ <- Monad.guard[({type m[a] = MaybeT[IO, a]})#m](isValid(s))//(MaybeT.monad[IO])
        } yield s

        def askPassword: MaybeT[IO, Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- getValidPassword
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        //MaybeT.runMaybeT(askPassword).unIO()
    }

    def _testMonadT {
        /*
        import Maybe.monadT

        def isValid(s: String): Boolean = true
        def getValidPassword: IO[Maybe[String]] = throw new Error

        def askPassword: IO[Maybe[Unit]] =
            IO.putStrLn >>= (_ =>
                getValidPassword >>= (_ =>
                    IO.putStrLn(

        for {
            _ <- IO.putStrLn("Insert your new password")
            value <- monadT(getValidPassword)
            _ <- IO.putStrLn("Storing in database...")
        } yield ()
        */
    }

}
