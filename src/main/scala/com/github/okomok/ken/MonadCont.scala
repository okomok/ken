

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadCont[m[+_]] extends Monad[m] {
    final val asMonadCont: MonadCont[apply] = this

    // Core
    //
    def callCC[a, b](f: (a => m[b]) => m[a]): m[a]
}


trait MonadContProxy[m[+_]] extends MonadCont[m] with MonadProxy[m] {
    override def self: MonadCont[m]

    override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = self.callCC(f)
}


object MonadCont {
    def apply[m <: Kind.Function1](implicit i: MonadCont[m#apply]): MonadCont[m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadCont[ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadCont[nt#apply] = new MonadCont[nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val self = Monad.deriving[nt, ot](i, j)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = j.new1 {
            i.callCC { (c: a => ot#apply[b]) =>
                j.old1 { f( a => j.new1(c(a)) ) }
            }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit i: MonadCont[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadCont[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
