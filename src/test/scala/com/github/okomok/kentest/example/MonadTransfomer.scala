

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest.example


class MonadTransformerTezt { // extends org.scalatest.junit.JUnit3Suite {

    // From: http://en.wikibooks.org/wiki/Haskell/Monad_transformers

    import com.github.okomok.ken._

    // Strongly-typed monad; Haskell way.
    def testStrongMonadT {
        import IO.MaybeT

        // Pull the monad explicitly.
        val m = MonadPlus[MaybeT.type]
        import m._

        val mt = MonadTrans[MaybeT.type]
        import mt.lift

        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

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

    // Weakly-typed monad; Power of Scala
    def testWeakMonadT {
        import IO.MaybeT

        val wm = MaybeT.weak.MonadPlus
        import wm._

        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        // No wrappers, no lifts.
        def getValidPassword: IO[Maybe[String_]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Just()

        // No runs
        askPassword.unIO()
    }
}
