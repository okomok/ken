

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2011, Mikhail Vorozhtsov, Bas van Dijk
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadBase[b[+_], m[+_]] extends Monad[m] {
    final val asMonadBase: MonadBase[b, apply1] = this

    type StM[+a]
    trait RunInBase {
        def apply[c](m: m[c]): b[StM[c]]
    }

    // Core
    //
    def baseMonad: Monad[b]
    def liftBase[a](b: b[a]): m[a] = liftBaseWith(_ => b)
    def liftBaseWith[a](f: RunInBase => b[a]): m[a]
    def restoreM[a](St: StM[a]): m[a]

    // Extra
    //
    def control[a](f: RunInBase => b[StM[a]]): m[a] = liftBaseWith(f) >>= ((St: StM[a]) => restoreM(St))

    def liftBaseOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = g => control { runInBase => f { a => runInBase(g(a)) } }
    def liftBaseOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = m => control { runInBase => f { runInBase(m) } }
    def liftBaseDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = m => liftBaseWith { runInBase => f { baseMonad.void { runInBase(m) } } }

    // Helper
    //
    final def liftBaseDefault[a, t[_[+_], +_]](b: b[a])(implicit _T: MonadTrans[t]): t[m, a] = _T.lift(liftBase(b))(this)
}


trait MonadBaseProxy[b[+_], m[+_]] extends MonadBase[b, m] with MonadProxy[m] {
    val selfMonadBase: MonadBase[b, m]
    override def selfMonad: Monad[m] = selfMonadBase
    override type StM[+a] = selfMonadBase.StM[a]

    private[this] def run2run(run: selfMonadBase.RunInBase): RunInBase = new RunInBase {
        override def apply[c](m: m[c]): b[StM[c]] = run(m)
    }

    override def baseMonad: Monad[b] = selfMonadBase.baseMonad
    override def liftBase[a](b: b[a]): m[a] = selfMonadBase.liftBase(b)
    override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = selfMonadBase.liftBaseWith(run => f(run2run(run)))
    override def restoreM[a](St: StM[a]): m[a] = selfMonadBase.restoreM(St)

    override def liftBaseOp[a, q, c](f: (a => b[StM[q]]) => b[StM[c]]): (a => m[q]) => m[c] = selfMonadBase.liftBaseOp(f)
    override def liftBaseOp_[a, q](f: b[StM[a]] => b[StM[q]]): m[a] => m[q] = selfMonadBase.liftBaseOp_(f)
    override def liftBaseDiscard[a](f: b[Unit] => b[a]): m[Unit] => m[a] = selfMonadBase.liftBaseDiscard(f)
}


object MonadBase extends MonadBaseInstance {
    def apply[b <: Kind.Function1, m <: Kind.Function1](implicit _M: MonadBase[b#apply1, m#apply1]): MonadBase[b#apply1, m#apply1] = _M

    // Helper
    final class TransDefault[t[_[+_], +_], n[+_], b[+_]](val _T: MonadTrans[t], val _N: MonadBase[b, n], _M: Monad[({type L[+a] = t[n, a]})#L]) extends MonadBase[b, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        final case class StM[+a](override val old: _N.StM[_T.StT[a]]) extends NewtypeOf[_N.StM[_T.StT[a]]]
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def baseMonad = _N.baseMonad
        override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = {
            _T.liftWith(run1 =>
                _N.liftBaseWith { runInBase =>
                    f {
                        new RunInBase {
                            override def apply[c](m: m[c]): b[StM[c]] = baseMonad.liftM((x: _N.StM[_T.StT[c]]) => StM(x))(runInBase(run1(m)(_N)))
                        }
                    }
                }
            )(_N)
        }
        override def restoreM[a](St: StM[a]): m[a] = _T.restoreT(_N.restoreM(St.old))(_N)
    }
}


sealed trait MonadBaseInstance { this: MonadBase.type =>
    implicit def _ofSame[m[+_]](implicit _M: Monad[m]): MonadBase[m, m] = new MonadBase[m, m] with MonadProxy[m] {
        private type b[+a] = m[a]
        override type StM[+a] = a
        override def selfMonad = _M
        override def baseMonad = _M
        override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = f {
            new RunInBase {
                override def apply[a](m: m[a]): b[StM[a]] = m
            }
        }
        override def restoreM[a](St: StM[a]): m[a] = _M.`return`(St)
    }
}
