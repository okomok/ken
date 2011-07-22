

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait IO[+a] {
    def unIO(): a
}


trait IOProxy[+a] extends IO[a] with Proxy {
    override def self: IO[a]
    override def unIO(): a = self.unIO()
}


object IO extends MonadIO[IO] {
    def apply[a](x: => a): IO[a] = new IO[a] {
        override def unIO = x
    }

// Overrides
    private[this] type m[+a] = IO[a]
    // Monad
    override def `return`[a](x: => a): m[a] = IO { x }
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = IO {
        y(x.unIO()).unIO()
    }
    // MonadIO
    def liftIO[a](io: IO[a]): m[a] = io

// Instances
    implicit val monad: MonadIO[IO] = this

// Output functions
    val putChar: Char => IO[Unit] = c => IO {
        Predef.print(c)
    }

    val putStr: String_ => IO[Unit] = s => IO {
        List.foreach(Predef.print)(s)
    }

    val putStrLn: String_ => IO[Unit] = s => {
        for { _ <- putStr(s); _ <- putChar('\n') } yield ()
    }

    def print[a](x: a)(implicit i: Show[a]): IO[Unit] = putStrLn(i.show(x))

// Input functions
    val getChar: IO[Char] = IO {
        Predef.readChar()
    }

    val getLine: IO[String_] = IO {
        val str = Predef.readLine()
        if (str == null) {
            throw new java.io.EOFException("getLine")
        } else {
            str
        }
    }

    val getContents: IO[String_] = {
        for { s <- getLine } yield (s ::: getContents.unIO)
    }

    val interact: (String_ => String_) => IO[Unit] = f => {
        for { s <- getContents; * <- putStr(f(s)) } yield *
    }

// Files
    type FilePath = String

    val readFile: FilePath => IO[String_] = f => IO {
        scala.io.Source.fromFile(f)
    }

    val writeFile: FilePath => String_ => IO[Unit] = f => txt => IO {
        val fw = new java.io.FileWriter(f)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

    val appendFile: FilePath => String_ => IO[Unit] = f => txt => IO {
        val fw = new java.io.FileWriter(f, true)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

// Exception handling in the I/O monad
    type IOError = java.io.IOException

    val ioError: IOError => IO[Nothing] = err => IO { throw err }

    val userError: String_ => IOError = s => new java.io.IOException(s.toString)

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = IO {
        try {
            io.unIO()
        } catch {
            case err: IOError => h(err).unIO()
        }
    }
}
