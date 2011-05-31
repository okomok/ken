

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait IO[a] {
    def unIO(): a
}

object IO extends Monad[IO] {
    implicit val theInstance = IO

    private[this] type f[a] = IO[a]
    // Applicative
    override def pure[a](x: => a): f[a] = new IO[a] {
        override def unIO(): a = x
    }
    // Monad
    override def op_>>=[a, b](x: f[a])(y: a => f[b]): f[b] = y(x.unIO())
}
