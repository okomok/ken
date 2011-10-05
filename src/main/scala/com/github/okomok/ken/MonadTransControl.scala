

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTransControl[t[_[+_], +_]] extends MonadTrans[t] {
    type Run = MonadTransControl.Run[t]

    final val asMonadTransControl: MonadTransControl[t] = this

    // Core
    //
    def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a]

    // Extra
    //
    final def control[n[+_], a](f: Run => n[t[n, a]])(implicit i: Monad[n], j: Monad[({type m[+a] = t[n, a]})#m]): t[n, a] = j.join(liftControl(f))
}


trait MonadTransControlProxy[t[_[+_], +_]] extends MonadTransControl[t] with MonadTransProxy[t] {
    def selfMonadTransControl: MonadTransControl[t]
    override def selfMonadTrans: MonadTrans[t] = selfMonadTransControl

    override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = selfMonadTransControl.liftControl(f)(i)
}


object MonadTransControl {
    def apply[t <: Kind.MonadTrans](implicit i: MonadTransControl[t#monadTrans]): MonadTransControl[t#monadTrans] = i
/*
    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTransControl](implicit j: Newtype1[nt#apply, ot#apply], i: MonadTransControl[ot#apply, ot#innerMonad, ot#baseResult]): MonadTransControl[nt#apply, ot#innerMonad, ot#baseResult] = new MonadTransControl[nt#apply, ot#innerMonad, ot#baseResult] with MonadTransProxy[nt#apply, ot#innerMonad] {
        type n[+a] = ot#innerMonad[a]
        type m[+a] = nt#apply[a]
        type u[+a] = ot#baseResult[a]
        override val selfMonadTrans = MonadTrans.deriving[nt, ot]

        override def liftControl[a](f: Run => n[a]): m[a] = j.newOf {
            i.liftControl { run =>
                f {
                    new Run {
                        override def apply[o[+_], b](t: m[b])(implicit mo: Monad[o]): n[o[u[b]]] = run[o, b](j.oldOf(t))
                    }
                }
            }
        }
    }

    def weak[nt <: Kind.MonadTransControl](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadTransControl[nt#apply, nt#innerMonad, nt#baseResult]): MonadTransControl[nt#oldtype1, nt#innerMonad, nt#baseResult] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)
*/
    // Run types
    //
    trait Run[t[_[+_], +_]] {
        def apply[n_[+_], o[+_], b](t: t[n_, b])(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]]
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
