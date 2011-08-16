

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
    def apply[s, m <: Kind.Function1](implicit i: MonadState[s, m#apply]): MonadState[s, m#apply] = i

    def deriving[s, nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadState[s, ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadState[s, nt#apply] = new MonadState[s, nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val self = Monad.deriving[nt, ot](i, j)
        override def get: m[s] = j.newOf { i.get }
        override def put(s: s): m[Unit] = j.newOf { i.put(s) }
    }

    def weak[s, nt <: Kind.Newtype1](implicit i: MonadState[s, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadState[s, nt#oldtype1] = deriving[s, Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
