

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
    type ask = m[r]
    def ask: ask

    def local[a](f: r => r)(m: m[a]): m[a]

    // Extra
    //
    def asks[a](f: r => a): m[a] = for { r <- ask } yield f(r)
}


trait MonadReaderProxy[r, m[+_]] extends MonadReader[r, m] with MonadProxy[m] {
    type selfMonadReader = MonadReader[r, m]
    def selfMonadReader: selfMonadReader
    override def selfMonad: selfMonad = selfMonadReader

    override def ask: ask = selfMonadReader.ask
    override def local[a](f: r => r)(m: m[a]): m[a] = selfMonadReader.local(f)(m)

    override def asks[a](f: r => a): m[a] = selfMonadReader.asks(f)
}


object MonadReader extends MonadReaderInstance {
    def apply[r, m <: Kind.Function1](implicit i: MonadReader[r, m#apply1]): MonadReader[r, m#apply1] = i

    def deriving[r, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadReader[r, nt#oldtype1]): MonadReader[r, nt#apply1] = new MonadReader[r, nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override def ask: ask = j.newOf { i.ask }
        override def local[a](f: r => r)(m: m[a]): m[a] = j.newOf { i.local(f)(j.oldOf(m)) }

        override def asks[a](f: r => a): m[a] = j.newOf { i.asks(f) }
    }

    def weak[r, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadReader[r, nt#apply1]): MonadReader[r, nt#oldtype1] = deriving[r, Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadReaderInstance { this: MonadReader.type =>
}
