

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {

    def testMonadT {
        val m = MaybeT.monad[IO]
        import m.`for`

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

}
