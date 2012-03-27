

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// @pending("nonstandard")
trait MonadBaseControl[b[+_], m[+_]] extends MonadBase[b, m] {
    final val asMonadBaseControl: MonadBaseControl[b, apply1] = this

    type StM[+a]
    trait RunInBase {
        def apply[c](m: m[c]): b[StM[c]]
    }

    // Core
    //
    def liftBaseWith[a](f: RunInBase => b[a]): m[a]
    def restoreM[a](St: StM[a]): m[a]

    // Overrides
    //
    // MonadBase
    override def liftBase[a](b: b[a]): m[a] = liftBaseWith(_ => b)

    // Extra
    //
    def control[a](f: RunInBase => b[StM[a]]): m[a] = liftBaseWith(f) >>= ((St: StM[a]) => restoreM(St))

    def liftBaseOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = g => control { runInBase => f { a => runInBase(g(a)) } }
    def liftBaseOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = m => control { runInBase => f { runInBase(m) } }
    def liftBaseDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = m => liftBaseWith { runInBase => f { baseMonad.void { runInBase(m) } } }
}


trait MonadBaseControlProxy[b[+_], m[+_]] extends MonadBaseControl[b, m] with MonadBaseProxy[b, m] {
    type selfMonadBaseControl = MonadBaseControl[b, m]
    val selfMonadBaseControl: selfMonadBaseControl
    override def selfMonadBase: selfMonadBase = selfMonadBaseControl

    override type StM[+a] = selfMonadBaseControl.StM[a]

    override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = selfMonadBaseControl.liftBaseWith(run => f(run2run(run)))
    override def restoreM[a](St: StM[a]): m[a] = selfMonadBaseControl.restoreM(St)

    override def liftBaseOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = selfMonadBaseControl.liftBaseOp(f)
    override def liftBaseOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = selfMonadBaseControl.liftBaseOp_(f)
    override def liftBaseDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = selfMonadBaseControl.liftBaseDiscard(f)

    private[this] def run2run(run: selfMonadBaseControl.RunInBase): RunInBase = new RunInBase {
        override def apply[c](m: m[c]): b[StM[c]] = run(m)
    }
}


object MonadBaseControl {
    def apply[b <: Kind.Function1, m <: Kind.Function1](implicit _M: MonadBaseControl[b#apply1, m#apply1]): MonadBaseControl[b#apply1, m#apply1] = _M
}
