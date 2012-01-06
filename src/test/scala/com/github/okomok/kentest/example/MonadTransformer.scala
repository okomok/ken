

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


class MonadTransformerTezt { // extends org.scalatest.junit.JUnit3Suite {

    // From: http://en.wikibooks.org/wiki/Haskell/Monad_transformers

    import com.github.okomok.ken._

    def isValid(s: String): Boolean = Eq[Kind.const[String]].op_===(s)("valid")

    // Strongly-typed monad; Haskell way.
    def testStrongMonadT {
        // Pull the monad explicitly.
        val m = MonadPlus[MaybeT.apply[IO.type]]
        import m._

        val mt = MonadTransControl[MaybeT.type]
        import mt.lift

        def getValidPassword: m.apply[String] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: m.apply[Unit] = for {
            _ <- lift { IO.putStrLn("Insert your new password") }
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift { IO.putStrLn("Storing in database...") }
        } yield ()

        askPassword.run.!
    }
/*
    // Weakly-typed monad; Power of Scala (any pitfall?)
    def testWeakMonadT {
        val wm = MonadPlus.weak[MaybeT.apply[IO]]
        import wm._ // hides the default IO monad.

        // No wrappers, no lifts.
        def getValidPassword: IO[Maybe[String]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Just() // lift finally.

        // No runs
        askPassword.!
    }
*/
}
