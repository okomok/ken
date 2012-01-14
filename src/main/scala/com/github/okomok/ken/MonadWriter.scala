

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


trait MonadWriter[m[+_]] extends Monad[m] {
    final val asMonadWriter: MonadWriter[apply1] = this

    // Core
    //
    type WriteType

    type monoid = Monoid[WriteType]
    def monoid: monoid

    type tell = WriteType => m[Unit]
    def tell: tell

    def listen[a](x: m[a]): m[(a, WriteType)]
    def pass[a](x: m[(a, WriteType => WriteType)]): m[a]

    // Extra
    //
    def listens[a, b](f: WriteType => b)(m: m[a]): m[(a, b)] = for { (a, w) <- listen(m) } yield (a, f(w))
    def censor[a](f: WriteType => WriteType)(m: m[a]): m[a] = pass { for { a <- m } yield (a, f) }
}


trait MonadWriterProxy[m[+_]] extends MonadWriter[m] with MonadProxy[m] {
    type selfMonadWriter = MonadWriter[m]
    val selfMonadWriter: selfMonadWriter
    override def selfMonad: selfMonad = selfMonadWriter

    override type WriteType = selfMonadWriter.WriteType
    override def monoid: monoid = selfMonadWriter.monoid
    override def tell: tell = selfMonadWriter.tell
    override def listen[a](x: m[a]): m[(a, WriteType)] = selfMonadWriter.listen(x)
    override def pass[a](x: m[(a, WriteType => WriteType)]): m[a] = selfMonadWriter.pass(x)

    override def listens[a, b](f: WriteType => b)(m: m[a]): m[(a, b)] = selfMonadWriter.listens(f)(m)
    override def censor[a](f: WriteType => WriteType)(m: m[a]): m[a] = selfMonadWriter.censor(f)(m)
}


object MonadWriter {
    type Of[w, m[+a]] = MonadWriter[m] { type WriteType = w }

    def apply[m <: Kind.Function1](implicit _M: MonadWriter[m#apply1]): MonadWriter.Of[_M.WriteType, m#apply1] = _M

    def deriving[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadWriter[nt#oldtype1]): MonadWriter.Of[_M.WriteType, nt#apply1] = new MonadWriter[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override type WriteType = _M.WriteType
        override def monoid: monoid = _M.monoid
        override val tell: tell = x => _Nt.newOf { _M.tell(x) }
        override def listen[a](x: m[a]): m[(a, WriteType)] = _Nt.newOf { _M.listen(_Nt.oldOf(x)) }
        override def pass[a](x: m[(a, WriteType => WriteType)]): m[a] = _Nt.newOf { _M.pass(_Nt.oldOf(x)) }
    }

    def weak[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadWriter[nt#apply1]): MonadWriter[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](_Nt.coNewtype, _M)
}
