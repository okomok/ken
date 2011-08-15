

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[m[+_]] extends Monad[m] with Alternative[m] {
    final val asMonadPlus: MonadPlus[apply] = this

    // Core
    //
    def mzero: m[Nothing]
    def mplus[a](x: m[a])(y: => m[a]): m[a]

    // Overrides
    //
    // Alternative
    override def empty: m[Nothing] = mzero
    override def op_<|>[a](x: m[a])(y: => m[a]): m[a] = mplus(x)(y)

    // Extra
    //
    def guard(b: Bool): m[Unit] = b match {
        case True => `return`() // one-element
        case False => mzero // empty
    }

    def msum[a](xs: List[m[a]]): m[a] = List.foldr(mplus[a])(mzero)(xs)

    // Infix
    //
    sealed class Infix_mplus[a](x: m[a]) {
        def _mplus_(y: => m[a]): m[a] = mplus(x)(y)
    }
    final implicit def _mplus_[a](x: m[a]): Infix_mplus[a] = new Infix_mplus(x)
}


trait MonadPlusProxy[m[+_]] extends MonadPlus[m] with MonadProxy[m] with AlternativeProxy[m] {
    override def self: MonadPlus[m]

    override def mzero: m[Nothing] = self.mzero
    override def mplus[a](x: m[a])(y: => m[a]): m[a] = self.mplus(x)(y)

    override def guard(b: Bool): m[Unit] = self.guard(b)
    override def msum[a](xs: List[m[a]]): m[a] = msum(xs)
}


object MonadPlus {
    def apply[m <: Kind.Function1](implicit i: MonadPlus[m#apply]): MonadPlus[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadPlus[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadPlus[nt#apply] = new MonadPlus[nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val self = Monad.deriving[nt, ot](i, j)
        override def mzero: m[Nothing] = j.new1 { i.mzero }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = j.new1 { i.mplus(j.old1(x))(j.old1(y)) }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadPlus[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadPlus[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
