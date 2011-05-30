

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Prelude {

    def id[a](x: a): a = x

    def const[a](x: a)(y: Any): a = x

    def apply[a, b](x: a => b)(y: a): b = x(y)

    def flip[a, b, c](x: a => b => c)(y: b)(z: a): c = x(z)(y)

    def putChar(x: Char): IO[Unit] = print(x)

    def putStr(x: String): IO[Unit] = print(x)

    def putStrLn(x: String): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.println(x)
    }

    def print(x: Any): IO[Unit] = new IO[Unit] {
        override def unIO(): Unit = Predef.print(x)
    }

    def getChar: IO[Char] = new IO[Char] {
        override def unIO(): Char = Predef.readChar()
    }

    def getLine: IO[String] = new IO[String] {
        override def unIO(): String = Predef.readLine()
    }

}
