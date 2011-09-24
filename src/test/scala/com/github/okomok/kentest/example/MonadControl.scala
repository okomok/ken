

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// See: http://www.yesodweb.com/blog/2011/08/monad-control


class MonadControlTest extends org.scalatest.junit.JUnit3Suite {
    def testDummy {}

    type MyError = String
    val MyError = List.from("my error")

    val mt = MonadTransControl[IO.ErrorT.apply[MyError]]
    val me = MonadError[MyError, IO.ErrorT.apply[MyError]]

    def simple {
        val sayHi: IO[Unit] = IO.putStrLn("Hello")
        val sayHiError: IO.ErrorT[MyError, Unit] = mt.lift { IO.putStrLn("Hello") }
    }

    def withMyFile[a](f: IO.Handle => IO[a]): IO[a] = IO.ioError(IO.userError("todo"))

    def slightlyMoreComplicated {
        val sayHi: IO.Handle => IO[Unit] = handle => IO.hPutStrLn(handle)("Hi there")
        val useMyFile: IO[Unit] = withMyFile(sayHi)
    }

    val sayHiError: IO.Handle => IO.ErrorT[MyError, Unit] = handle => {
        import me.`for`
        for {
            _ <- mt.lift { IO.hPutStrLn(handle)("Hi there, error!") }
            * <- me.throwError(MyError)
        } yield *
    }

    // val useMyFileErrorBad: IO.ErrorT[MyError, Unit] = withMyFile(sayHiError) // doesn't compile.

    val useMyFileError: IO.ErrorT[MyError, Unit] = {
        val unwrapped: IO.Handle => IO[Either[MyError, Unit]] = handle => IO.ErrorT.run {
            sayHiError(handle)
        }
        val applied: IO[Either[MyError, Unit]] = withMyFile(unwrapped)
        val rewrapped: IO.ErrorT[MyError, Unit] = IO.ErrorT(applied)
        rewrapped
    }

    val useMyFileError6: IO.ErrorT[MyError, Unit] = mt.control { run =>
        withMyFile { h =>
            for { // This `for` is IO.for.
                u <- run[IO, Unit](sayHiError(h))
            } yield IO.ErrorT(u) // makes it dependent...
            // IO.liftM((x: IO[Either[MyError, Unit]]) => IO.ErrorT(x))(run[IO, Unit](sayHiError(h)))
        }
    }

    // Weak way of Scala
    //
    val mtw = MonadTransControl.weak[IO.ErrorT.apply[MyError]]
    implicit val mew = MonadError.weak[MyError, IO.ErrorT.apply[MyError]]

    val sayHiErrorWeak: IO.Handle => IO[Either[MyError, Unit]] = handle => {
        import mew.`for`
        for {
            _ <- IO.hPutStrLn(handle)("Hi there, error!")
            * <- mew.throwError(MyError)
        } yield *
    }

    val useMyFileErrorWeak: IO[Either[MyError, Unit]] = mtw.control { run =>
        withMyFile { h =>
            run[IO, Unit](sayHiErrorWeak(h))
        }
    }
}