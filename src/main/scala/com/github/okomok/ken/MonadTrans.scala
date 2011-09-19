

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Lifts an inner monad.
 */
trait MonadTrans[n[+_], m[+_]] extends Typeclass with Kind.AbstractMonadTrans {
    override type apply1[+a] = m[a]
    override type innerMonad[+a] = n[a]

    def lift[a](n: n[a]): m[a]
}


trait MonadTransProxy[n[+_], m[+_]] extends MonadTrans[n, m] {
    def selfMonadTrans: MonadTrans[n, m]

    override def lift[a](n: n[a]): m[a] = selfMonadTrans.lift(n)
}


object MonadTrans {
    def apply[m <: Kind.MonadTrans](implicit i: MonadTrans[m#innerMonad, m#apply]): MonadTrans[m#innerMonad, m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTrans](implicit i: MonadTrans[ot#innerMonad, ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadTrans[ot#innerMonad, nt#apply] = new MonadTrans[ot#innerMonad, nt#apply] {
        private type n[+a] = ot#innerMonad[a]
        private type m[+a] = nt#apply[a]
        override def lift[a](n: n[a]): m[a] = j.newOf { i.lift(n) }
    }

    def weak[nt <: Kind.MonadTrans](implicit i: MonadTrans[nt#innerMonad, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadTrans[nt#innerMonad, nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
