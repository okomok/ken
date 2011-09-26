

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


trait MonadPlus[m[+_]] extends Monad[m] with Alternative[m] {
    final val asMonadPlus: MonadPlus[apply] = this

    // Core
    //
    def mzero: m[Nothing]
    def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a]

    // Overrides
    //
    // Alternative
    override def empty: m[Nothing] = mzero
    override def op_<|>[a](x: m[a])(y: Lazy[m[a]]): m[a] = mplus(x)(y)

    // Extra
    //
    def guard(b: Bool): m[Unit] = b match {
        case True => `return`() // one-element
        case False => mzero // empty
    }

    def msum[a](xs: List[m[a]]): m[a] = List.foldr(mplus[a])(mzero)(xs)

    // Operators
    //
    private[ken] sealed class Op_mplus[a](x: m[a]) {
        def _mplus_(y: Lazy[m[a]]): m[a] = mplus(x)(y)
    }
    final implicit def _mplus_[a](x: m[a]): Op_mplus[a] = new Op_mplus(x)
}


trait MonadPlusProxy[m[+_]] extends MonadPlus[m] with MonadProxy[m] with AlternativeProxy[m] {
    def selfMonadPlus: MonadPlus[m]
    override def selfMonad: Monad[m] = selfMonadPlus
    override def selfAlternative: Alternative[m] = selfMonadPlus

    override def mzero: m[Nothing] = selfMonadPlus.mzero
    override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = selfMonadPlus.mplus(x)(y)

    override def guard(b: Bool): m[Unit] = selfMonadPlus.guard(b)
    override def msum[a](xs: List[m[a]]): m[a] = msum(xs)
}


object MonadPlus extends MonadPlusInstance {
    def apply[m <: Kind.Function1](implicit i: MonadPlus[m#apply]): MonadPlus[m#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit i: MonadPlus[nt#oldtype1], j: Newtype1[nt#apply, nt#oldtype1]): MonadPlus[nt#apply] = new MonadPlus[nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt](i, j)

        override def mzero: m[Nothing] = j.newOf { i.mzero }
        override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = j.newOf { i.mplus(j.oldOf(x))(j.oldOf(y)) }

        override def guard(b: Bool): m[Unit] = j.newOf { i.guard(b) }
        override def msum[a](xs: List[m[a]]): m[a] = j.newOf { i.msum( for { nt <- xs } yield j.oldOf(nt) ) }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadPlus[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadPlus[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](i, j.coNewtype)
}


sealed trait MonadPlusInstance { this: MonadPlus.type =>
}
