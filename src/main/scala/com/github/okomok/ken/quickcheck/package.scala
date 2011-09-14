

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


package object quickcheck {

    // Exception
    //
    def tryEvaluate[a](x: a): IO[Either[SomeException, a]] = tryEvaluateIO(IO.`return`(x))

    def tryEvaluateIO[a](m: IO[a]): IO[Either[SomeException, a]] = {
        import IO.>>=
        SomeException.`try`(m >>= Exception.evaluate)
    }

    // Str
    //
    def ranges[a](k: a)(n: a)(implicit i: Integral[a]): Str = {
        import i._
        val n_ = k * (n _div_ k)
        Str(Show.show(n_) ++: " -- " ++: Show.show(n_ + k - 1))
    }

    val number: Int => String => String = n => s => {
        Show.show(n) ++: " " ++: s ++: List.from(if (n == 1) "" else "s")
    }

    val short: Int => String => String = n => s => {
        val k = List.length(s)
        val i = if (n >= 5) 3 else 0
        if (n < k) List.take(n-2-i)(s) ++: ".." ++: List.drop(k-i)(s)
        else s
    }

    def showErr[a](x: a)(implicit i: Show[a]): String = List.unwords(List.words(i.show(x)))

    val bold: String => String = s => s

    import RealWorld._
    //import IO.`for`

    val newTerminal: IO[Terminal] = {
        for {
            _ <- IO.hFlush(IO.stdout)
            _ <- IO.hFlush(IO.stderr)
            ref <- newIORef(IO.`return`())
        } yield Terminal(ref)
    }

    val flush: Terminal => IO[Unit] = { case Terminal(ref) =>
        for {
            io <- readIORef(ref)
            _ <- writeIORef(ref)(IO.`return`())
            * <- io
        } yield *
    }

    val postpone: Terminal => IO[Unit] => IO[Unit] = { case Terminal(ref) => io_ =>
        for {
            io <- readIORef(ref)
            * <- writeIORef(ref)(io >> io_)
        } yield *
    }

    val putPart: Terminal => String => IO[Unit] = tm => s => {
        for {
            _ <- flush(tm)
            _ <- IO.putStr(s)
            * <- IO.hFlush(IO.stdout)
        } yield *
    }

    val putTemp: Terminal => String => IO[Unit] = tm => s => {
        val h = IO.stderr
        for {
            _ <- flush(tm)
            _ <- IO.hPutStr(h)(s)
            _ <- IO.hPutStr(h)(for { _ <- s } yield '\b')
            _ <- IO.hFlush(h)
            * <- postpone(tm) {
                for {
                    * <- IO.hPutStr(h) {
                        (for { _ <- s } yield ' ') ++: (for { _ <- s } yield '\b')
                    }
                } yield *
            }
        } yield *
    }

    val putLine: Terminal => String => IO[Unit] = tm => s => {
        for {
            _ <- flush(tm)
            _ <- IO.putStrLn(s)
            * <- IO.hFlush(IO.stdout)
        } yield *
    }
}
