

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
}
