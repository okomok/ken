

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


// type IO[+a] = RealWorld.ST[a]
final case class IO[+a](override val get: IORep[a]) extends Strong[IORep[a]] {
    def ! : a = get.apply(RealWorld)._1
}


object IO extends MonadIO[IO] with ThisIsInstance {
    // Overrides
    //
    private type m[+a] = IO[a]
    // Monad
    override def `return`[a](x: Lazy[a]): m[a] = returnIO(x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = bindIO(m)(k)
    // MonadIO
    def liftIO[a](io: IO[a]): m[a] = io

    private def returnIO[a](x: Lazy[a]): IO[a] = IO { s => (x, s) }

    private def bindIO[a, b](m: IO[a])(k: a => IO[b]): IO[b] = IO { s =>
        unIO(m)(s) match {
            case (a, new_s) => unIO(k(a))(new_s)
        }
    }

    // UnsafeIO operations
    //
    def unsafeIO[a](x: => a): IO[a] = IO { s => (x, s) }

    def unIO[a](io: IO[a]): IORep[a] = io.get

    def unsafePerformIO[a](io: IO[a]): a = unsafeDupablePerformIO(noDuplicate(io))

    def unsafeDupablePerformIO[a](io: IO[a]): a = unIO(io)(RealWorld) match {
        case (r, _) => r
    }

    def noDuplicate[a](io: IO[a]): IO[a] = IO { s =>
        s.synchronized {
            unIO(io)(s)
        }
    }

    // Output functions
    //
    val putChar: Char => IO[Unit] = c => unsafeIO {
        Predef.print(c)
    }

    val putStr: String => IO[Unit] = s => unsafeIO {
        s.foreach(Predef.print)
    }

    val putStrLn: String => IO[Unit] = s => {
        for { _ <- putStr(s); _ <- putChar('\n') } yield ()
    }

    def print[a](x: a)(implicit i: Show[a]): IO[Unit] = putStrLn(i.show(x))

    // Input functions
    //
    val getChar: IO[Char] = unsafeIO {
        Predef.readChar()
    }

    val getLine: IO[String] = unsafeIO {
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
        for { s <- getContents; * <- putStr(f(s)) } yield *
    }

    // Files
    //
    type FilePath = Predef.String

    val readFile: FilePath => IO[String] = f => unsafeIO {
        scala.io.Source.fromFile(f)
    }

    val writeFile: FilePath => String => IO[Unit] = f => txt => unsafeIO {
        val fw = new java.io.FileWriter(f)
        try {
            fw.write(List.toJString(txt))
        } finally {
            fw.close()
        }
    }

    val appendFile: FilePath => String => IO[Unit] = f => txt => unsafeIO {
        val fw = new java.io.FileWriter(f, True)
        try {
            fw.write(List.toJString(txt))
        } finally {
            fw.close()
        }
    }

    // Exception handling in the I/O asMonad
    //
    val ioError: IOError => IO[Nothing] = err => unsafeIO { throw err }

    val userError: String => IOError = s => new java.io.IOException(s.toString)

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = unsafeIO {
        try {
            io.!
        } catch {
            case err: IOError => h(err).!
        }
    }

    // Handle
    //
    final case class Handle(rep: Any)

    val stdout: Handle = Handle(java.lang.System.out)
    val stderr: Handle = Handle(java.lang.System.err)

    val hClose: Handle => IO[Unit] = {
        case Handle(rep: java.io.InputStream) => unsafeIO { rep.close() }
        case Handle(rep: java.io.OutputStream) => unsafeIO { rep.close() }
    }

    val hFlush: Handle => IO[Unit] = {
        case Handle(rep: java.io.OutputStream) => unsafeIO { rep.flush() }
        case _ => `return`()
    }

    val hPutStr: Handle => String => IO[Unit] = {
        case Handle(rep: java.io.PrintStream) => s => unsafeIO { rep.print(List.toJString(s)) }
        case _ => _ => `return`()
    }
}
