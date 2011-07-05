

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Test {
    val i = Applicative.ofFunction1[Int]
    import i.<@>

    val r: Int => Int => Int = ((x: Int) => (y: Int) => x + y) <@> ((x: Int) => x)


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
