

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Lifts an inner monad.
 */
trait MonadTrans[n[+_], m[+_]] extends Typeclass with Kind.MonadTrans {
    override type innerMonad[+a] = n[a]
    override type apply1[+a] = m[a]

    final val asMonadTrans: MonadTrans[innerMonad, apply] = this

    def lift[a](n: n[a]): m[a]
}


trait MonadTransProxy[n[+_], m[+_]] extends MonadTrans[n, m] {
    def selfMonadTrans: MonadTrans[n, m]

    override def lift[a](n: n[a]): m[a] = selfMonadTrans.lift(n)
}


object MonadTrans extends MonadTransInstance {
    def apply[m <: Kind.MonadTrans](implicit i: MonadTrans[m#innerMonad, m#apply]): MonadTrans[m#innerMonad, m#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTrans](implicit j: Newtype1[nt#apply, ot#apply], i: MonadTrans[ot#innerMonad, ot#apply]): MonadTrans[ot#innerMonad, nt#apply] = new MonadTrans[ot#innerMonad, nt#apply] {
        private type n[+a] = ot#innerMonad[a]
        private type m[+a] = nt#apply[a]
        override def lift[a](n: n[a]): m[a] = j.newOf { i.lift(n) }
    }

    def weak[nt <: Kind.MonadTrans](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadTrans[nt#innerMonad, nt#apply]): MonadTrans[nt#innerMonad, nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)
}


sealed trait MonadTransInstance { this: MonadTrans.type =>
}
