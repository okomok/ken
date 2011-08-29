

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


trait MonadWriter[w, m[+_]] extends Monad[m] {
    final val asMonadWriter: MonadWriter[w, apply] = this

    // Core
    //
    def monoid: Monoid[w]
    def tell(x: w): m[Unit]
    def listen[a](x: m[a]): m[(a, w)]
    def pass[a](x: m[(a, w => w)]): m[a]

    // Extra
    //
    def listens[a, b](f: w => b)(m: m[a]): m[(a, b)] = for { (a, w) <- listen(m) } yield (a, f(w))
    def censor[a](f: w => w)(m: m[a]): m[a] = pass { for { a <- m } yield (a, f) }
}


trait MonadWriterProxy[w, m[+_]] extends MonadWriter[w, m] with MonadProxy[m] {
    def selfMonadWriter: MonadWriter[w, m]
    override def selfMonad: Monad[m] = selfMonadWriter

    override def monoid: Monoid[w] = selfMonadWriter.monoid
    override def tell(x: w): m[Unit] = selfMonadWriter.tell(x)
    override def listen[a](x: m[a]): m[(a, w)] = selfMonadWriter.listen(x)
    override def pass[a](x: m[(a, w => w)]): m[a] = selfMonadWriter.pass(x)

    override def listens[a, b](f: w => b)(m: m[a]): m[(a, b)] = selfMonadWriter.listens(f)(m)
    override def censor[a](f: w => w)(m: m[a]): m[a] = selfMonadWriter.censor(f)(m)
}


object MonadWriter {
    def apply[w, m[+_]](implicit i: MonadWriter[w, m]) = i

    def deriving[w, nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadWriter[w, ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadWriter[w, nt#apply] = new MonadWriter[w, nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt, ot](i, j)
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = j.newOf { i.tell(x) }
        override def listen[a](x: m[a]): m[(a, w)] = j.newOf { i.listen(j.oldOf(x)) }
        override def pass[a](x: m[(a, w => w)]): m[a] = j.newOf { i.pass(j.oldOf(x)) }
    }

    def weak[w, nt <: Kind.Newtype1](implicit i: MonadWriter[w, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadWriter[w, nt#oldtype1] = deriving[w, Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
