

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Monad.`for`


trait IO[+a] {
    def unIO(): a
}


trait IOProxy[+a] extends IO[a] with Proxy {
    def self: IO[a]
    override def unIO(): a = self.unIO()
}


object IO {
    implicit object theInstance extends Monad[IO] {
        private[this] type m[a] = IO[a]
        override def `return`[a](x: => a): m[a] = new IO[a] {
            override def unIO(): a = x
        }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = y(x.unIO())
    }

// Output functions
    def putChar(c: Char): IO[Unit] = print(c)

    def putStr(s: List[Char]): IO[Unit] = new IO[Unit] {
        // avoid `stringize` to print lazily.
        override def unIO(): Unit = List.foreach(Predef.print)(s)
    }

    def putStrLn(s: List[Char]): IO[Unit] = {
        for (_ <- putStr(s); _ <- putChar('\n')) yield ()
    }

    def print[a](x: a): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.print(x)
    }

// Input functions
    def getChar: IO[Char] = new IO[Char] {
        override def unIO(): Char = Predef.readChar()
    }

    def getLine: IO[List[Char]] = new IO[List[Char]] {
        override def unIO(): List[Char] = {
            val str = Predef.readLine()
            if (str == null) {
                throw new java.io.EOFException("getLine")
            } else {
                str
            }
        }
    }

    def getContents: IO[List[Char]] = {
        for (s <- getLine) yield (s ::: getContents.unIO)
    }

    def interact(f: List[Char] => List[Char]): IO[Unit] = {
        for (s <- getContents) {
            putStr(f(s))
        }
    }

// Files
    type FilePath = String

    def readFile(f: FilePath): IO[List[Char]] = new IO[List[Char]] {
        override def unIO(): List[Char] = scala.io.Source.fromFile(f)
    }

    def writeFile(f: FilePath)(txt: List[Char]): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = {
            val fw = new java.io.FileWriter(f)
            try {
                fw.write(List.stringize(txt))
            } finally {
                fw.close()
            }
        }
    }

    def appendFile(f: FilePath)(txt: List[Char]): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = {
            val fw = new java.io.FileWriter(f, true)
            try {
                fw.write(List.stringize(txt))
            } finally {
                fw.close()
            }
        }
    }

// Exception handling in the I/O monad
    type IOError = java.io.IOException

    def ioError(err: IOError): IO[Nothing] = new IO[Nothing] {
        override def unIO(): Nothing = throw err
    }

    def userError(str: String): IOError = new java.io.IOException(str)

    def `catch`[a](io: IO[a])(h: IOError => IO[a]): IO[a] = new IO[a] {
        override def unIO(): a = try {
            io.unIO()
        } catch {
            case err: IOError => h(err).unIO()
        }
    }
}
