

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._

object StackTraceString {
    def apply(): String = {
        var that: String = null
        try {
            throw new RuntimeException("StackTraceString")
        } catch {
            case t: Throwable => that = fromThrowable(t)
        }
        that
    }

    def fromThrowable(t: Throwable): String = {
        val w = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(w))
        w.toString
    }
}


class MaybeTest extends org.scalatest.junit.JUnit3Suite {
    /*
    def testStrongMonadTObsolete {
        import IO.MaybeT // MaybeT[a] <:< Identity[IO[Maybe[a]]]
        import MaybeT._monad._

        var valid = false
        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        def getValidPassword: MaybeT[String_] = {
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
            askPassword.run.unIO()
        }
    }
    */
    def testImplyMonadT2 {
        val wm = IO.MaybeT.weak.MonadPlus
        import wm._

        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        def getValidPassword: IO[Maybe[String_]] = for {
            s <- IO.getLine
            _ <- guard(isValid(s))
        } yield s

        def askPassword: IO[Maybe[Unit]] = for {
            _ <- IO.putStrLn("Insert your new password")
            value <- msum { List.repeat(getValidPassword) }
            _ <- IO.putStrLn("Storing in database...")
        } yield Just()

        ignore {
            askPassword.unIO()
        }
    }

    def testStrongMonadT2 {
        import IO.MaybeT

        val m = MonadPlus[MaybeT.type]
        import m._

        val mt = MonadTrans[MaybeT.type]
        import mt.lift

        var valid = false
        def isValid(s: String_): Boolean = Eq[String_].op_==(s)("valid")

        def getValidPassword: MaybeT[String_] = {
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
            askPassword.run.unIO()
        }
    }
}
