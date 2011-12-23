

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class IO[+a](override val old: IORep[a]) extends NewtypeOf[IORep[a]] {
    def ! : a = old(RealWorld)._1
}


object IO extends MonadIO[IO] with ThisIsInstance {
    // Overrides
    //
    private type m[+a] = IO[a]
    // Monad
    override def `return`[a](x: Lazy[a]): m[a] = returnIO(x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = bindIO(m)(k)
    // MonadIO
    override def liftIO[a](io: IO[a]): m[a] = io

    private def returnIO[a](x: => a): IO[a] = IO { s => IORep.done(x, s) }
    private def bindIO[a, b](m: IO[a])(k: a => IO[b]): IO[b] = IO { s => IORep.cont(unIO(m), (a: a) => unIO(k(a)), s) }

    // UnsafeIO operations
    //
    def unIO[a](io: IO[a]): IORep[a] = io.old

    def unsafePerformIO[a](io: IO[a]): a = unsafeDupablePerformIO(noDuplicate(io))

    def unsafeDupablePerformIO[a](io: IO[a]): a = unIO(io)(RealWorld)._1

    def noDuplicate[a](io: IO[a]): IO[a] = IO { s =>
        s.synchronized {
            unIO(io)(s)
        }
    }

    // Output functions
    //
    val putChar: Char => IO[Unit] = c => returnIO {
        Predef.print(c)
    }

    val putStr: String => IO[Unit] = s => returnIO {
        List.foreach(Predef.print)(s)
    }

    val putStrLn: String => IO[Unit] = s => {
        for { _ <- putStr(s); _ <- putChar('\n') } yield ()
    }

    def print[a](x: a)(implicit i: Show[a]): IO[Unit] = putStrLn(i.show(x))

    // Input functions
    //
    val getChar: IO[Char] = returnIO {
        Predef.readChar()
    }

    val getLine: IO[String] = returnIO {
        val str = Predef.readLine()
        if (str == null) {
            throw new java.io.EOFException("getLine")
        } else {
            str
        }
    }

    val getContents: IO[String] = {
        for { s <- getLine } yield (s ++: getContents.!)
    }

    val interact: (String => String) => IO[Unit] = f => {
        for { s <- getContents } { putStr(f(s)) }
    }

    // Files
    //
    type FilePath = JString

    val readFile: FilePath => IO[String] = f => returnIO {
        scala.io.Source.fromFile(f)
    }

    val writeFile: FilePath => String => IO[Unit] = f => txt => returnIO {
        val fw = new java.io.FileWriter(f)
        try {
            fw.write(txt.asJString)
        } finally {
            fw.close()
        }
    }

    val appendFile: FilePath => String => IO[Unit] = f => txt => returnIO {
        val fw = new java.io.FileWriter(f, True)
        try {
            fw.write(txt.asJString)
        } finally {
            fw.close()
        }
    }

    // Exception handling in the I/O asMonad
    //
    lazy val ioError: IOError => IO[Nothing] = err => IOError.throwIO(err)

    lazy val userError: String => IOError = str => IOError(new java.io.IOException(str.asJString))

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = IOError.`catch`(io)(h)

    def `try`[a](f: IO[a]): IO[Either[IOError, a]] = `catch`(for { r <- f } yield Right(r).of[IOError, a])(e =>`return`(Lazy(Left(e))))

    // Handle
    //
    final case class Handle(rep: java.io.Closeable)

    val stdout: Handle = Handle(java.lang.System.out)
    val stderr: Handle = Handle(java.lang.System.err)

    val hClose: Handle => IO[Unit] = {
        case Handle(rep) => returnIO { rep.close() }
    }

    val hFlush: Handle => IO[Unit] = {
        case Handle(rep: java.io.Flushable) => returnIO { rep.flush() }
        case _ => ioError(userError("non-flushable handle"))
    }

    def hPrint[a](h: Handle)(x: a)(implicit i: Show[a]): IO[Unit] = h match {
        case Handle(rep: java.io.PrintStream) => returnIO { rep.print(Show.show(x)) }
        case _ => ioError(userError("non-printable handle"))
    }

    val hPutStr: Handle => String => IO[Unit] = h => x => hPrint(h)(x)
    val hPutStrLn: Handle => String => IO[Unit] = h => s => hPutStr(h)(s ++: List.from("\n"))

    // StdGen
    //
    import RealWorld.IORef

    lazy val setStdGen: StdGen => IO[Unit] = sgen => IORef.write(theStdGen)(sgen)
    lazy val getStdGen: IO[StdGen] = IORef.read(theStdGen)

    lazy val theStdGen: IORef[StdGen] = unsafePerformIO {
        for {
            rng <- `return`(new StdGen())
        } {
            IORef.`new`(rng)
        }
    }

    lazy val newStdGen: IO[StdGen] = IORef.atomicModify(theStdGen)(StdGen.split)

    def getStdRandom[a](f: StdGen => (a, StdGen)): IO[a] = {
        val swap: Pair[a, StdGen] => (StdGen, a) = { case (v, g) => (g, v) }
        IORef.atomicModify(theStdGen)(swap `.` f)
    }
}
