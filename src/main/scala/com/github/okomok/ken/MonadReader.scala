

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


trait MonadReader[m[+_]] extends Monad[m] {
    final val asMonadReader: MonadReader[apply1] = this

    // Core
    //
    type ReadType

    type ask = m[ReadType]
    def ask: ask

    def local[a](f: ReadType => ReadType)(m: m[a]): m[a]

    // Extra
    //
    def asks[a](f: ReadType => a): m[a] = for { r <- ask } yield f(r)
}


trait MonadReaderProxy[m[+_]] extends MonadReader[m] with MonadProxy[m] {
    type selfMonadReader = MonadReader[m]
    val selfMonadReader: selfMonadReader
    override def selfMonad: selfMonad = selfMonadReader

    override type ReadType = selfMonadReader.ReadType
    override def ask: ask = selfMonadReader.ask
    override def local[a](f: ReadType => ReadType)(m: m[a]): m[a] = selfMonadReader.local(f)(m)

    override def asks[a](f: ReadType => a): m[a] = selfMonadReader.asks(f)
}


object MonadReader extends MonadReaderInstance {
    type Of[r, m[+a]] = MonadReader[m] { type ReadType = r }

    def apply[m <: Kind.Function1](implicit _M: MonadReader[m#apply1]): MonadReader.Of[_M.ReadType, m#apply1] = _M

    def deriving[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadReader[nt#oldtype1]): MonadReader.Of[_M.ReadType, nt#apply1] = new MonadReader[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override type ReadType = _M.ReadType
        override def ask: ask = _Nt.newOf { _M.ask }
        override def local[a](f: ReadType => ReadType)(m: m[a]): m[a] = _Nt.newOf { _M.local(f)(_Nt.oldOf(m)) }

        override def asks[a](f: ReadType => a): m[a] = _Nt.newOf { _M.asks(f) }
    }

    def weak[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadReader[nt#apply1]): MonadReader.Of[_M.ReadType, nt#oldtype1] = deriving[Kind.coNewtype1[nt]](_Nt.coNewtype, _M)
}


sealed trait MonadReaderInstance { this: MonadReader.type =>
}
