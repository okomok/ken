

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2011, Mikhail Vorozhtsov, Bas van Dijk
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// @pending("nonstandard")
trait MonadBase[b[+_], m[+_]] extends Monad[m] {
    final val asMonadBase: MonadBase[b, apply1] = this

    // Core
    //
    type baseMonad = Monad[b]
    def baseMonad: baseMonad

    def liftBase[a](b: b[a]): m[a]

    // Helper
    //
    final def liftBaseDefault[a, t[_[+_], +_]](b: b[a])(implicit _T: MonadTrans[t]): t[m, a] = _T.lift(liftBase(b))(this)
}


trait MonadBaseProxy[b[+_], m[+_]] extends MonadBase[b, m] with MonadProxy[m] {
    type selfMonadBase = MonadBase[b, m]
    def selfMonadBase: MonadBase[b, m]
    override def selfMonad: selfMonad = selfMonadBase

    override def baseMonad: baseMonad = selfMonadBase.baseMonad
    override def liftBase[a](b: b[a]): m[a] = selfMonadBase.liftBase(b)
}


object MonadBase extends MonadBaseInstance {
    def apply[b <: Kind.Function1, m <: Kind.Function1](implicit _M: MonadBase[b#apply1, m#apply1]): MonadBase[b#apply1, m#apply1] = _M
}


sealed trait MonadBaseInstance { this: MonadBase.type =>
    implicit def _ofSame[m[+_]](implicit _M: Monad[m]): MonadBaseControl[m, m] = new MonadBaseControl[m, m] with MonadProxy[m] {
        private type b[+a] = m[a]
        override type StM[+a] = a
        override def selfMonad: selfMonad = _M
        override def baseMonad: baseMonad = _M
        override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = f {
            new RunInBase {
                override def apply[a](m: m[a]): b[StM[a]] = m
            }
        }
        override def restoreM[a](St: StM[a]): m[a] = _M.`return`(St)
    }
}
