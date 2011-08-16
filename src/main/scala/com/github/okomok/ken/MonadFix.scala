

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadFix[m[+_]] extends Monad[m] {
    final val asMonadFix: MonadFix[apply] = this

    // Core
    //
    def mfix[a](f: (=> a) => m[a]): m[a]
}


trait MonadFixProxy[m[+_]] extends MonadFix[m] with MonadProxy[m] {
    override def self: MonadFix[m]

    override def mfix[a](f: (=> a) => m[a]): m[a] = self.mfix(f)
}


object MonadFix {
    def apply[m <: Kind.Function1](implicit i: MonadFix[m#apply]): MonadFix[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadFix[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadFix[nt#apply] = new MonadFix[nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val self = Monad.deriving[nt, ot](i, j)
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            def k(a: => a): ot#apply[a] = j.oldOf(f(a))
            j.newOf { i.mfix(k) }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadFix[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadFix[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
