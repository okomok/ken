

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTrans[t[_[+_], +_]] extends TypeclassLike with Kind.MonadTrans {
    type Run = MonadTrans.Run[t]
    override type monadTrans[n[+_], +a] = t[n, a]

    final val asMonadTrans: MonadTrans[monadTrans] = this

    // Core
    //
    def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a]
    def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a]

    // Extra
    //
    def control[n[+_], a](f: Run => n[t[n, a]])(implicit i: Monad[n], j: Monad[({type m[+a] = t[n, a]})#m]): t[n, a] = j.join(liftWith(f))
}


trait MonadTransProxy[t[_[+_], +_]] extends MonadTrans[t] {
    def selfMonadTrans: MonadTrans[t]

    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = selfMonadTrans.lift(n)
    override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = selfMonadTrans.liftWith(f)(i)

    override def control[n[+_], a](f: Run => n[t[n, a]])(implicit i: Monad[n], j: Monad[({type m[+a] = t[n, a]})#m]): t[n, a] = selfMonadTrans.control(f)(i, j)
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
    // Run types
    //
    trait Run[t[_[+_], +_]] {
        def apply[n_[+_], o[+_], b](t: t[n_, b], * : Type1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]]
    }

    trait RunInBase[m[+_], base[+_]] {
        def apply[b](t: m[b]): base[m[b]]
    }

    // Lifting
    //
    def idLiftControl[m[+_], a](f: RunInBase[m, m] => m[a])(implicit j: Monad[m]): m[a] = f {
        new RunInBase[m, m] {
            override def apply[b](t: m[b]): m[m[b]] = j.liftM[b, m[b]](j.`return`[b])(t)
        }
    }
}


sealed trait MonadTransInstance { this: MonadTrans.type =>
}
