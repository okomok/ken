

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadState[s, m[+_]] extends Monad[m] {
    final val asMonadState: MonadState[s, apply] = this

    // Core
    //
    def get: m[s]
    def put(s: s): m[Unit]

    // Extra
    //
    def modify(f: s => s): m[Unit] = for { s <- get; _ <- put(f(s)) } yield ()
    def gets[a](f: s => a): m[a] = for { s <- get } yield f(s)
}


trait MonadStateProxy[s, m[+_]] extends MonadState[s, m] with MonadProxy[m] {
    override def self: MonadState[s, m]

    override def get: m[s] = self.get
    override def put(s: s): m[Unit] = self.put(s)

    override def modify(f: s => s): m[Unit] = self.modify(f)
    override def gets[a](f: s => a): m[a] = self.gets(f)
}


object MonadState {
    def apply[s, m[+_]](implicit i: MonadState[s, m]): MonadState[s, m] = i
}
