

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.example


import com.github.okomok.ken._


// See: http://www.yesodweb.com/blog/2011/08/monad-control


class MonadControlTest extends org.scalatest.junit.JUnit3Suite {
    def testDummy {}

    type MyError = String
    val MyError = List.from("my error")

    val mt = MonadTransControl[ErrorT.apply1[MyError]]
    val me = MonadError[ErrorT.apply2[MyError, IO.type]]

    implicitly[me.ErrorType =:= MyError]

    def simple {
        val sayHi: IO[Unit] = IO.putStrLn("Hello")
        val sayHiError: ErrorT[MyError, IO, Unit] = mt.lift { IO.putStrLn("Hello") }
    }

    def withMyFile[a](f: IO.Handle => IO[a]): IO[a] = IO.ioError(IO.userError("todo"))

    def slightlyMoreComplicated {
        val sayHi: IO.Handle => IO[Unit] = handle => IO.hPutStrLn(handle)("Hi there")
        val useMyFile: IO[Unit] = withMyFile(sayHi)
    }

    val sayHiError: IO.Handle => ErrorT[MyError, IO, Unit] = handle => {
        import me.`for`
        for {
            _ <- mt.lift { IO.hPutStrLn(handle)("Hi there, error!") }
        } {
            me.throwError(MyError)
        }
    }

    val useMyFileError: ErrorT[MyError, IO, Unit] = {
        val unwrapped: IO.Handle => IO[Either[MyError, Unit]] = handle => ErrorT.run {
            sayHiError(handle)
        }
        val applied: IO[Either[MyError, Unit]] = withMyFile(unwrapped)
        val rewrapped: ErrorT[MyError, IO, Unit] = ErrorT(applied)
        rewrapped
    }
/*
    val useMyFileError6: ErrorT[MyError, IO, Unit] = mt.control { run =>
        withMyFile { h => run(sayHiError(h))(IO) }
    }
*/
    val mi = MonadIOControl[ErrorT.apply2[MyError, IO.type]]

    val useMyFileError7: ErrorT[MyError, IO, Unit] = mi.control { run =>
        withMyFile { h => run(sayHiError(h)) }
    }


/*
    No longer works.
    // Weak way of Scala
    //
    val mtw = MonadTransControl.weak[IO.ErrorT.apply[MyError]]
    implicit val mew = MonadError.weak[MyError, IO.ErrorT.apply[MyError]]

    val sayHiErrorWeak: IO.Handle => IO[Either[MyError, Unit]] = handle => {
        import mew.`for`
        for {
            _ <- IO.hPutStrLn(handle)("Hi there, error!")
        } {
            mew.throwError(MyError)
        }
    }

    val useMyFileErrorWeak: IO[Either[MyError, Unit]] = mtw.control { run =>
        withMyFile { h =>
            run[IO, Unit](sayHiErrorWeak(h))
        }
    }
*/
}
