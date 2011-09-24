

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadReader[r, m[+_]] extends Monad[m] {
    final val asMonadReader: MonadReader[r, apply] = this

    // Core
    //
    def ask: m[r]
    def local[a](f: r => r)(m: m[a]): m[a]

    // Extra
    //
    def asks[a](f: r => a): m[a] = for { r <- ask } yield f(r)
}


trait MonadReaderProxy[r, m[+_]] extends MonadReader[r, m] with MonadProxy[m] {
    def selfMonadReader: MonadReader[r, m]
    override def selfMonad: Monad[m] = selfMonadReader

    override def ask: m[r] = selfMonadReader.ask
    override def local[a](f: r => r)(m: m[a]): m[a] = selfMonadReader.local(f)(m)

    override def asks[a](f: r => a): m[a] = selfMonadReader.asks(f)
}


object MonadReader {
    def apply[r, m <: Kind.Function1](implicit i: MonadReader[r, m#apply]): MonadReader[r, m#apply] = i

    def deriving[r, nt <: Kind.Newtype1](implicit i: MonadReader[r, nt#oldtype1], j: Newtype1[nt#apply, nt#oldtype1]): MonadReader[r, nt#apply] = new MonadReader[r, nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt]

        override def ask: m[r] = j.newOf { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = j.newOf { i.local(f)(j.oldOf(m)) }

        override def asks[a](f: r => a): m[a] = j.newOf { i.asks(f) }
    }

    def weak[r, nt <: Kind.Newtype1](implicit i: MonadReader[r, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadReader[r, nt#oldtype1] = deriving[r, Kind.coNewtype1[nt]](i, j.coNewtype)
}
