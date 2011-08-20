

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest.example


class MonadTransformerTezt { // extends org.scalatest.junit.JUnit3Suite {

    // From: http://en.wikibooks.org/wiki/Haskell/Monad_transformers

    import com.github.okomok.ken._

    def isValid(s: String_): Boolean = Eq[Kind.const[String_]].op_==(s)("valid")

    // Strongly-typed monad; Haskell way.
    def testStrongMonadT {
        import IO.MaybeT

        // Pull the monad explicitly.
        val m = MonadPlus[MaybeT.type]
        import m._

        val mt = MonadTrans[MaybeT.type]
        import mt.lift


        def getValidPassword: MaybeT[String_] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: MaybeT[Unit] = for {
            _ <- lift { IO.putStrLn("Insert your new password") }
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift { IO.putStrLn("Storing in database...") }
        } yield ()

        askPassword.run.unIO()
    }

    // Weakly-typed monad; Power of Scala (any pitfall?)
    def testWeakMonadT {
        import IO.MaybeT

        val wm = MonadPlus.weak[MaybeT.type]
        import wm._ // IO[Maybe[_]] monad hides the default IO monad.

        // No wrappers, no lifts.
        def getValidPassword: IO[Maybe[String_]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Just() // lift finally.

        // No runs
        askPassword.unIO()
    }
}
