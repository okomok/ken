

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


class MonadTransformerTezt { // extends org.scalatest.junit.JUnit3Suite {

    // From: http://en.wikibooks.org/wiki/Haskell/Monad_transformers

    import com.github.okomok.ken._

    def isValid(s: String_): Boolean = Eq[Kind.const[String_]].op_===(s)("valid")

    // Strongly-typed monad; Haskell way.
    def testStrongMonadT {
        import IO.ErrorT

        // Pull the monad explicitly.
        val m = MonadPlus[ErrorT.apply[IOError]]
        import m._

        val mt = MonadTrans[ErrorT.apply[IOError]]
        import mt.lift

        def getValidPassword: ErrorT[IOError, String_] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: ErrorT[IOError, Unit] = for {
            _ <- lift { IO.putStrLn("Insert your new password") }
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift { IO.putStrLn("Storing in database...") }
        } yield ()

        askPassword.run.unIO()
    }

    // Weakly-typed monad; Power of Scala (any pitfall?)
    def testWeakMonadT {
        import IO.ErrorT

        val wm = MonadPlus.weak[ErrorT.apply[IOError]]
        import wm._ // hides the default IO monad.

        // No wrappers, no lifts.
        def getValidPassword: IO[Either[IOError, String_]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Either[IOError, Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Right() // lift finally.

        // No runs
        askPassword.unIO()
    }
}
