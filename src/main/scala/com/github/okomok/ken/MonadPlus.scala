

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


trait MonadPlus[m[+_]] extends Monad[m] with Alternative[m] { outer =>
    final val asMonadPlus: MonadPlus[apply1] = this

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

    // Misc
    //
    def filter[a](p: a => Bool)(m: m[a]): m[a] = op_>>=(m)((a: a) => if (p(a)) `return`(a) else mzero)

    // For-comprehension
    //
    private[this] def monadFor[a](m: m[a]): For[a] = super.`for`(m)

    @Annotation.compilerWorkaround("2.9.1", 5070)
    override implicit def `for`[a](m: m[a]): ken.For[m, a] = new ForProxy[a] {
        override val selfFor = monadFor(m)
        override def filter(p: a => Bool): m[a] = outer.filter(p)(m)
    }
}


trait MonadPlusProxy[m[+_]] extends MonadPlus[m] with MonadProxy[m] with AlternativeProxy[m] {
    def selfMonadPlus: MonadPlus[m]
    override def selfMonad: Monad[m] = selfMonadPlus
    override def selfAlternative: Alternative[m] = selfMonadPlus

    override def mzero: m[Nothing] = selfMonadPlus.mzero
    override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = selfMonadPlus.mplus(x)(y)

    override def guard(b: Bool): m[Unit] = selfMonadPlus.guard(b)
    override def msum[a](xs: List[m[a]]): m[a] = msum(xs)

    override def filter[a](p: a => Bool)(m: m[a]): m[a] = selfMonadPlus.filter(p)(m)
}


object MonadPlus extends MonadPlusInstance {
    def apply[m <: Kind.Function1](implicit i: MonadPlus[m#apply1]): MonadPlus[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadPlus[nt#oldtype1]): MonadPlus[nt#apply1] = new MonadPlus[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad = Monad.deriving[nt]

        override def mzero: m[Nothing] = j.newOf { i.mzero }
        override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = j.newOf { i.mplus(j.oldOf(x))(j.oldOf(y)) }

        override def guard(b: Bool): m[Unit] = j.newOf { i.guard(b) }
        override def msum[a](xs: List[m[a]]): m[a] = j.newOf { i.msum( for { nt <- xs } yield j.oldOf(nt) ) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadPlus[nt#apply1]): MonadPlus[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadPlusInstance { this: MonadPlus.type =>
}
