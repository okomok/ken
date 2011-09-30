

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {
    /*
    def testStrongMonadTObsolete {
        import IO.MaybeT // MaybeT[a] <:< Identity[IO[Maybe[a]]]
        import MaybeT._monad._

        var valid = false
        def isValid(s: String): Boolean = Eq[Kind.const[String]].op_===(s)("valid")

        def getValidPassword: MaybeT[String] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: MaybeT[Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        ignore {
            askPassword.run.!
        }
    }
    */
    def testWeakMonadT2 {
        val wm = MonadPlus.weak[IO.MaybeT.type]
        import wm._

        val wmt = MonadTrans.weak[IO.MaybeT.type]
        import wmt.lift

        def isValid(s: String): Boolean = Eq[Kind.const[String]].op_===(s)("valid")

        def getValidPassword: IO[Maybe[String]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- lift { IO.putStrLn("Insert your new password") }
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift { IO.putStrLn("Storing in database...") }
        } yield ()

        def askPassword2: IO[Maybe[Unit]] = {
            IO.putStrLn("Insert your new password") >>= { (_: Unit) =>
                // IO[Unit]
                msum { List.repeat(getValidPassword) } >>= { (value: String) =>
                    // IO[Maybe[String]]
                    IO.putStrLn("Storing in database...") >>= { (_: Unit) =>
                        // IO[Unit]
                        wm.`return`(())
                    }
                }
            }
        }

        ignore {
            askPassword.!
        }
    }

    def testStrongMonadT2 {
        import IO.MaybeT

        val m = MonadPlus[MaybeT.type]
        import m._

        val mt = MonadTrans[MaybeT.type]
        import mt.lift

        var valid = false
        def isValid(s: String): Boolean = Eq[Kind.const[String]].op_===(s)("valid")

        def getValidPassword: MaybeT[String] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: MaybeT[Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        ignore {
            askPassword.run.!
        }
    }
}
