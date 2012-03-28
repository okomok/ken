

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2011, Mikhail Vorozhtsov, Bas van Dijk
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


// @pending("nonstandard")
trait MonadInner[b[+_], m[+_]] extends Monad[m] {
    final val asMonadInner: MonadInner[b, apply1] = this

    // Core
    //
    type innerMonad = Monad[b]
    def innerMonad: innerMonad

    def liftInner[a](b: b[a]): m[a]

    // Helper
    //
    final def liftInnerDefault[a, t[_[+_], +_]](b: b[a])(implicit _T: MonadTrans[t]): t[m, a] = _T.lift(liftInner(b))(this)
}


trait MonadInnerProxy[b[+_], m[+_]] extends MonadInner[b, m] with MonadProxy[m] {
    type selfMonadInner = MonadInner[b, m]
    def selfMonadInner: MonadInner[b, m]
    override def selfMonad: selfMonad = selfMonadInner

    override def innerMonad: innerMonad = selfMonadInner.innerMonad
    override def liftInner[a](b: b[a]): m[a] = selfMonadInner.liftInner(b)
}


object MonadInner extends MonadInnerInstance {
    def apply[b <: Kind.Function1, m <: Kind.Function1](implicit _M: MonadInner[b#apply1, m#apply1]): MonadInner[b#apply1, m#apply1] = _M
}


sealed trait MonadInnerInstance { this: MonadInner.type =>
    implicit def _ofSame[m[+_]](implicit _M: Monad[m]): MonadInnerControl[m, m] = new MonadInnerControl[m, m] with MonadProxy[m] {
        private type b[+a] = m[a]
        override type StM[+a] = a
        override def selfMonad: selfMonad = _M
        override def innerMonad: innerMonad = _M
        override def liftInnerWith[a](f: RunInInner => b[a]): m[a] = f {
            new RunInInner {
                override def apply[a](m: m[a]): b[StM[a]] = m
            }
        }
        override def restoreM[a](St: StM[a]): m[a] = _M.`return`(St)
    }
}
