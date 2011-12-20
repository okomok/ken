

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTrans[t[_[+_], +_]] extends TypeclassLike with Kind.MonadTrans {
    override type monadTrans[n[+_], +a] = t[n, a]
    final val asMonadTrans: MonadTrans[monadTrans] = this

    type StT[+a]
    trait Run {
        def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]]
    }

    // Core
    //
    def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a] = liftWith(_ => n)(_N)
    def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a]
    def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a]
}


trait MonadTransProxy[t[_[+_], +_]] extends MonadTrans[t] {
    val selfMonadTrans: MonadTrans[t]
    override type StT[+a] = selfMonadTrans.StT[a]

    private[this] def run2run(run: selfMonadTrans.Run): Run = new Run {
        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = run(t)
    }

    override def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a] = selfMonadTrans.lift(n)
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = selfMonadTrans.liftWith((run: selfMonadTrans.Run) => f(run2run(run)))(_N)
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = selfMonadTrans.restoreT(nSt)(_N)
}


object MonadTrans extends MonadTransInstance {
    def apply[t <: Kind.MonadTrans](implicit _T: MonadTrans[t#monadTrans]): MonadTrans[t#monadTrans] = _T

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
