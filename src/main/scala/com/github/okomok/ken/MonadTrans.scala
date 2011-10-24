

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTrans[t[_[+_], +_]] extends TypeclassLike with Kind.MonadTrans {
    override type monadTrans[n[+_], +a] = t[n, a]

    final val asMonadTrans: MonadTrans[monadTrans] = this

    def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a]
}


trait MonadTransProxy[t[_[+_], +_]] extends MonadTrans[t] {
    def selfMonadTrans: MonadTrans[t]

    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = selfMonadTrans.lift(n)
}


object MonadTrans extends MonadTransInstance {
    def apply[t <: Kind.MonadTrans](implicit i: MonadTrans[t#monadTrans]): MonadTrans[t#monadTrans] = i
/*
    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTrans](implicit j: Newtype1[nt#apply1, ot#apply1], i: MonadTrans[ot#apply1, ot#innerMonad]): MonadTrans[nt#apply1, ot#innerMonad] = new MonadTrans[nt#apply1, ot#innerMonad] {
        private type n[+a] = ot#innerMonad[a]
        private type m[+a] = nt#apply1[a]
        override def lift[a](n: n[a]): m[a] = j.newOf { i.lift(n) }
    }

    def weak[nt <: Kind.MonadTrans](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadTrans[nt#apply1, nt#innerMonad]): MonadTrans[nt#oldtype1, nt#innerMonad] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)
*/
}


sealed trait MonadTransInstance { this: MonadTrans.type =>
}
