

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait IO[+a] {
    def unIO(): a
}

object IO extends Monad[IO] {
    implicit val theInstance = this

    private[this] type m[a] = IO[a]
    override def `return`[a](x: => a): m[a] = new IO[a] {
        override def unIO(): a = x
    }
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = y(x.unIO())

// Basic I/O operations
    def putChar(x: Char): IO[Unit] = print(x)

    def putStr(x: String): IO[Unit] = print(x)

    def putStrLn(x: String): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.println(x)
    }

    def print(x: Any): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.print(x)
    }

    val getChar: IO[Char] = new IO[Char] {
        override def unIO(): Char = Predef.readChar()
    }

    val getLine: IO[String] = new IO[String] {
        override def unIO(): String = Predef.readLine()
    }
}
