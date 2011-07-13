

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadState[s, m[+_]] extends Monad[m] { outer =>
    def get: m[s]
    def put(s: s): m[Unit]

    final def modify(f: s => s): m[Unit] = for { s <- get; _ <- put(f(s)) } yield ()

    final def gets[a](f: s => a): m[a] = for { s <- get } yield f(s)
}

object MonadState {
    def apply[s, m[+_]](implicit i: MonadState[s, m]): MonadState[s, m] = i
}
