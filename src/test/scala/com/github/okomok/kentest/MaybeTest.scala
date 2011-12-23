

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class MaybeTest extends org.scalatest.junit.JUnit3Suite {

    def testEq {
        expect(true)(Eq.op_===(Nothing)(Nothing))
        expect(false)(Eq.op_===(Just(1))(Just(2)))
        expect(true)(Eq.op_===(Just(10))(Just(10)))
    }

    def testOrd {
        val i = Ord[Maybe[Int]]
        import i._
        expect(false)(Nothing < Nothing)
        expect(true)(Just(1) < Just(2))
        expect(true)(Nothing < Just(10))
        expect(false)(Nothing > Just(10))
    }

    def testShow {
        val i = Show[Maybe[String]]
        expect("Nothing")(i.show(Nothing).asJString)
        expect("Just(\"abcd\")")(i.show(Just[String]("abcd")).asJString)
    }

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

/*
    def testWeakMonadT2 {
        val wm = MonadPlus.weak[MaybeT.apply[IO]]
        import wm._

        val wmt = MonadTransControl.weak[MaybeT.apply[IO]]
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
*/

    def testStrongMonadT2 {

        val m = MonadPlus[MaybeT.apply[IO.type]]
        import m._

        val mt = MonadTransControl[MaybeT.type]
        import mt.lift

        var valid = false
        def isValid(s: String): Boolean = Eq[Kind.const[String]].op_===(s)("valid")

        def getValidPassword: m.apply[String] = {
            for {
                s <- lift(IO.getLine)
                _ <- guard(isValid(s))
            } yield s
        }

        def askPassword: m.apply[Unit] = for {
            _ <- lift(IO.putStrLn("Insert your new password"))
            value <- msum { List.repeat(getValidPassword) }
            _ <- lift(IO.putStrLn("Storing in database..."))
        } yield ()

        ignore {
            askPassword.run.!
        }
    }
}
