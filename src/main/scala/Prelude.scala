

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Prelude {

    def id[a](x: a): a = x

    def const[a](x: a): Any => a = { _ => x }

    def compose[a, b, c](f: b => c)(g: a => b): a => c = { x => f(g(x)) }

    def flip[a, b, c](f: a => b => c): b => a => c = { x => y => f(y)(x) }

    def apply[a, b](f: a => b)(x: a): b = f(x)

    def until[a](p: a => Boolean)(f: a => a)(x: a): a = {
        if (p(x)) x else until(p)(f)(f(x))
    }

    def asTypeOf[a](x: a): a => a = const(x)

    def error(msg: String): Nothing = throw new Error(msg)

    def undefined: Nothing = throw new Error

    val show: Any => String = { x => x.toString }

    val putChar: Char => IO[Unit] = print(_)

    val putStr: String => IO[Unit] = print(_)

    val putStrLn: String => IO[Unit] = { x =>
        new IO[Unit] {
            override def unIO(): Unit = Predef.println(x)
        }
    }

    val print: Any => IO[Unit] = { x =>
        new IO[Unit] {
            override def unIO(): Unit = Predef.print(x)
        }
    }

    val getChar: IO[Char] = new IO[Char] {
        override def unIO(): Char = Predef.readChar()
    }

    val getLine: IO[String] = new IO[String] {
        override def unIO(): String = Predef.readLine()
    }

}
