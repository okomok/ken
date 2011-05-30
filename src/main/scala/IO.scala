

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait IO[a] {
    def unIO(): a
}


object IO extends Monad {
    // Functor
    override type f_[a] = IO[a]
    // Applicative
    override def pure[a](x: => a): f_[a] = new IO[a] {
        override def unIO(): a = x
    }
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = y(x.unIO())
}
