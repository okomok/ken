

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

    // Coercions between IO and ST
    //
    def stToIO[a](st: RealWorld.ST[a]): IO[a] = st match {
        case RealWorld.ST(m) => IO(m)
    }

    def ioToST[a](io: IO[a]): RealWorld.ST[a] = io match {
        case IO(m) => RealWorld.ST(m)
    }

    // Output functions
    //
    val putChar: Char => IO[Unit] = c => unsafeIO {
        Predef.print(c)
    }

    val putStr: String_ => IO[Unit] = s => unsafeIO {
        s.foreach(Predef.print)
    }

    val putStrLn: String_ => IO[Unit] = s => {
        for { _ <- putStr(s); _ <- putChar('\n') } yield ()
    }

    def print[a](x: a)(implicit i: Show[a]): IO[Unit] = putStrLn(i.show(x))

    // Input functions
    //
    val getChar: IO[Char] = unsafeIO {
        Predef.readChar()
    }

    val getLine: IO[String_] = unsafeIO {
        val str = Predef.readLine()
        if (str == null) {
            throw new java.io.EOFException("getLine")
        } else {
            str
        }
    }

    val getContents: IO[String_] = {
        for { s <- getLine } yield (s ++: getContents.!)
    }

    val interact: (String_ => String_) => IO[Unit] = f => {
        for { s <- getContents; * <- putStr(f(s)) } yield *
    }

    // Files
    //
    type FilePath = String

    val readFile: FilePath => IO[String_] = f => unsafeIO {
        scala.io.Source.fromFile(f)
    }

    val writeFile: FilePath => String_ => IO[Unit] = f => txt => unsafeIO {
        val fw = new java.io.FileWriter(f)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

    val appendFile: FilePath => String_ => IO[Unit] = f => txt => unsafeIO {
        val fw = new java.io.FileWriter(f, True)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

    // Exception handling in the I/O asMonad
    //
    val ioError: IOError => IO[Nothing] = err => unsafeIO { throw err }

    val userError: String_ => IOError = s => new java.io.IOException(s.toString)

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = unsafeIO {
        try {
            io.!
        } catch {
            case err: IOError => h(err).!
        }
    }
}
