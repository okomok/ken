

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// @pending("nonstandard")
trait MonadInnerControl[b[+_], m[+_]] extends MonadInner[b, m] {
    final val asMonadInnerControl: MonadInnerControl[b, apply1] = this

    type StM[+a]
    trait RunInInner {
        def apply[c](m: m[c]): b[StM[c]]
    }

    // Core
    //
    def liftInnerWith[a](f: RunInInner => b[a]): m[a]
    def restoreM[a](St: StM[a]): m[a]

    // Overrides
    //
    // MonadInner
    override def liftInner[a](b: b[a]): m[a] = liftInnerWith(_ => b)

    // Extra
    //
    def control[a](f: RunInInner => b[StM[a]]): m[a] = liftInnerWith(f) >>= ((St: StM[a]) => restoreM(St))

    def liftInnerOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = g => control { runInInner => f { a => runInInner(g(a)) } }
    def liftInnerOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = m => control { runInInner => f { runInInner(m) } }
    def liftInnerDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = m => liftInnerWith { runInInner => f { innerMonad.void { runInInner(m) } } }
}


trait MonadInnerControlProxy[b[+_], m[+_]] extends MonadInnerControl[b, m] with MonadInnerProxy[b, m] {
    type selfMonadInnerControl = MonadInnerControl[b, m]
    val selfMonadInnerControl: selfMonadInnerControl
    override def selfMonadInner: selfMonadInner = selfMonadInnerControl

    override type StM[+a] = selfMonadInnerControl.StM[a]

    override def liftInnerWith[a](f: RunInInner => b[a]): m[a] = selfMonadInnerControl.liftInnerWith(run => f(run2run(run)))
    override def restoreM[a](St: StM[a]): m[a] = selfMonadInnerControl.restoreM(St)

    override def liftInnerOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = selfMonadInnerControl.liftInnerOp(f)
    override def liftInnerOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = selfMonadInnerControl.liftInnerOp_(f)
    override def liftInnerDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = selfMonadInnerControl.liftInnerDiscard(f)

    private[this] def run2run(run: selfMonadInnerControl.RunInInner): RunInInner = new RunInInner {
        override def apply[c](m: m[c]): b[StM[c]] = run(m)
    }
}


object MonadInnerControl {
    def apply[b <: Kind.Function1, m <: Kind.Function1](implicit _M: MonadInnerControl[b#apply1, m#apply1]): MonadInnerControl[b#apply1, m#apply1] = _M
}
