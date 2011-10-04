

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTransX[t[_[+_], +_]] extends Typeclass with Kind.MonadTransX {
    override type monadTrans[n[+_], +a] = t[n, a]

    final val asMonadTransX: MonadTransX[monadTrans] = this

    def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a]
}


trait MonadTransXProxy[t[_[+_], +_]] extends MonadTransX[t] {
    def selfMonadTransX: MonadTransX[t]

    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = selfMonadTransX.lift(n)
}


object MonadTransX extends MonadTransXInstance {
    def apply[t <: Kind.MonadTransX](implicit i: MonadTransX[t#monadTrans]): MonadTransX[t#monadTrans] = i
/*
    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTransX](implicit j: Newtype1[nt#apply, ot#apply], i: MonadTransX[ot#apply, ot#innerMonad]): MonadTransX[nt#apply, ot#innerMonad] = new MonadTransX[nt#apply, ot#innerMonad] {
        private type n[+a] = ot#innerMonad[a]
        private type m[+a] = nt#apply[a]
        override def lift[a](n: n[a]): m[a] = j.newOf { i.lift(n) }
    }

    def weak[nt <: Kind.MonadTransX](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadTransX[nt#apply, nt#innerMonad]): MonadTransX[nt#oldtype1, nt#innerMonad] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)
*/
}


sealed trait MonadTransXInstance { this: MonadTransX.type =>
}
