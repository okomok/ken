

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
    override implicit def instance = this

    def apply[a](x: => a): IO[a] = new IO[a] {
        override def unIO = x
    }

// Instance
    private[this] type m[+a] = IO[a]
    // Monad
    override def `return`[a](x: a): m[a] = IO { x }
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = IO {
        y(x.unIO()).unIO()
    }
    // MonadIO
    def liftIO[a](io: IO[a]): m[a] = io

// Output functions
    def putChar(c: Char): IO[Unit] = print(c)

    def putStr(s: String_): IO[Unit] = IO {
        List.foreach(Predef.print)(s)
    }

    def putStrLn(s: String_): IO[Unit] = {
        for { _ <- putStr(s); _ <- putChar('\n') } yield ()
    }

    def print[a](x: a): IO[Unit] = IO {
        Predef.print(x)
    }

    def printLn[a](x: a): IO[Unit] = IO {
        Predef.println(x)
    }

// Input functions
    def getChar: IO[Char] = IO {
        Predef.readChar()
    }

    def getLine: IO[String_] = IO {
        val str = Predef.readLine()
        if (str == null) {
            throw new java.io.EOFException("getLine")
        } else {
            str
        }
    }

    def getContents: IO[String_] = {
        for { s <- getLine } yield (s ::: getContents.unIO)
    }

    def interact(f: String_ => String_): IO[Unit] = {
        for { s <- getContents; r <- putStr(f(s)) } yield r
    }

// Files
    type FilePath = String

    def readFile(f: FilePath): IO[String_] = IO {
        scala.io.Source.fromFile(f)
    }

    def writeFile(f: FilePath)(txt: String_): IO[Unit] = IO {
        val fw = new java.io.FileWriter(f)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

    def appendFile(f: FilePath)(txt: String_): IO[Unit] = IO {
        val fw = new java.io.FileWriter(f, true)
        try {
            fw.write(List.stringize(txt))
        } finally {
            fw.close()
        }
    }

// Exception handling in the I/O monad
    type IOError = java.io.IOException

    def ioError(err: IOError): IO[Nothing] = IO {
        throw err
    }

    def userError(str: String): IOError = new java.io.IOException(str)

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = IO {
        try {
            io.unIO()
        } catch {
            case err: IOError => h(err).unIO()
        }
    }
}
