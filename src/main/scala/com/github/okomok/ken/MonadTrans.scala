

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Lifts an inner monad.
 */
trait MonadTrans[m[+_], n[+_]] extends Typeclass with Kind.MonadTrans {
    override type innerMonad[+a] = n[a]
    override type apply1[+a] = m[a]

    final val asMonadTrans: MonadTrans[apply, innerMonad] = this

    def lift[a](n: n[a]): m[a]
}


trait MonadTransProxy[m[+_], n[+_]] extends MonadTrans[m, n] {
    def selfMonadTrans: MonadTrans[m, n]

    override def lift[a](n: n[a]): m[a] = selfMonadTrans.lift(n)
}


object MonadTrans extends MonadTransInstance {
    def apply[m <: Kind.MonadTrans](implicit i: MonadTrans[m#apply, m#innerMonad]): MonadTrans[m#apply, m#innerMonad] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTrans](implicit j: Newtype1[nt#apply, ot#apply], i: MonadTrans[ot#apply, ot#innerMonad]): MonadTrans[nt#apply, ot#innerMonad] = new MonadTrans[nt#apply, ot#innerMonad] {
        private type n[+a] = ot#innerMonad[a]
        private type m[+a] = nt#apply[a]
        override def lift[a](n: n[a]): m[a] = j.newOf { i.lift(n) }
    }

    def weak[nt <: Kind.MonadTrans](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadTrans[nt#apply, nt#innerMonad]): MonadTrans[nt#oldtype1, nt#innerMonad] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)
}


sealed trait MonadTransInstance { this: MonadTrans.type =>
}
