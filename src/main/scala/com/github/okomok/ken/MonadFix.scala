

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadFix[m[+_]] extends Monad[m] {
    final val asMonadFix: MonadFix[apply] = this

    // Core
    //
    def mfix[a](f: Lazy[a] => m[a]): m[a]
}


trait MonadFixProxy[m[+_]] extends MonadFix[m] with MonadProxy[m] {
    def selfMonadFix: MonadFix[m]
    override def selfMonad: Monad[m] = selfMonadFix

    override def mfix[a](f: Lazy[a] => m[a]): m[a] = selfMonadFix.mfix(f)
}


object MonadFix extends MonadFixInstance {
    def apply[m <: Kind.Function1](implicit i: MonadFix[m#apply1]): MonadFix[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadFix[nt#oldtype1]): MonadFix[nt#apply1] = new MonadFix[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad = Monad.deriving[nt]

        override def mfix[a](f: Lazy[a] => m[a]): m[a] = {
            val k: Lazy[a] => nt#oldtype1[a] = a => j.oldOf(f(a))
            j.newOf { i.mfix(k) }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadFix[nt#apply1]): MonadFix[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadFixInstance { this: MonadFix.type =>
}
