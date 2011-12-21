

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTrans[t[_[+_], +_]] extends TypeclassLike with Kind.MonadTrans { outer =>
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

    // Default implementations
    //
    final def defaultMonadBase[n[+_], b[+_]](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = t[n, a]})#L] = new MonadBase[b, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        final case class StM[+a](override val old: _N.StM[outer.StT[a]]) extends NewtypeOf[_N.StM[outer.StT[a]]]
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def baseMonad = _N.baseMonad
        override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = outer.liftWith(run1 =>
            _N.liftBaseWith { runInBase =>
                f {
                    new RunInBase {
                        override def apply[c](m: m[c]): b[StM[c]] = baseMonad.liftM((x: _N.StM[outer.StT[c]]) => StM(x))(runInBase(run1(m)(_N)))
                    }
                }
            }
        )(_N)
        override def restoreM[a](St: StM[a]): m[a] = outer.restoreT(_N.restoreM(St.old))(_N)
    }

    final def defaultMonadCont[n[+_]](_M: MonadBase[n, ({type L[+a] = t[n, a]})#L], _N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = error("how?")
    }

    final def defaultMonadError[n[+_], e](_M: MonadBase[n, ({type L[+a] = t[n, a]})#L], _N: MonadError[e, n]): MonadError[e, ({type L[+a] = t[n, a]})#L] = new MonadError[e, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def throwError[a](e: e): m[a] = outer.lift(_N.throwError(e))(_N)
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _M.control { run =>
            _N.catchError(run(m)) { e => run(h(e)) }
        }
    }

    final def defaultMonadFix[n[+_]](_M: MonadBase[n, ({type L[+a] = t[n, a]})#L], _N: MonadFix[n]): MonadFix[({type L[+a] = t[n, a]})#L] = new MonadFix[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = _M.control { run =>
            val k: Lazy[_M.StM[a]] => n[_M.StM[a]] = st => {
                run(_M.op_>>=(_M.restoreM(st.!))(a => f(a)))
            }
            _N.mfix(k)
        }
    }

    final def defaultMonadIO[n[+_]](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadIO[n]): MonadIO[({type L[+a] = t[n, a]})#L] = new MonadIO[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override def liftIO[a](io: IO[a]): m[a] = outer.lift(_N.liftIO(io))(_N)
    }

    final def defaultMonadPlus[n[+_]](_M: MonadBase[n, ({type L[+a] = t[n, a]})#L], _N: MonadPlus[n]): MonadPlus[({type L[+a] = t[n, a]})#L] = new MonadPlus[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override val mzero: mzero = outer.lift(_N.mzero)(_N)
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _M.control { run =>
            _N.mplus(run(m))(run(n.!))
        }
    }

    final def defaultMonadReader[n[+_], r](_M: MonadBase[n, ({type L[+a] = t[n, a]})#L], _N: MonadReader[r, n]): MonadReader[r, ({type L[+a] = t[n, a]})#L] = new MonadReader[r, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override val ask: m[r] = outer.lift(_N.ask)(_N)
        override def local[a](f: r => r)(m: m[a]): m[a] = _M.control { run =>
            _N.local(f)(run(m))
        }
    }

    final def defaultMonadState[n[+_], s](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadState[s, n]): MonadState[s, ({type L[+a] = t[n, a]})#L] = new MonadState[s, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad = _M
        override val get: get = outer.lift(_N.get)(_N)
        override val put: put = s => outer.lift(_N.put(s))(_N)
    }
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

    // Default implementation providers
    //
    type AnyMonad = Monad.type ^: MonadBase.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil

    import Kind.List.Contains

    // No extra parameters
    //
    trait Deriving0[t[_[+_], +_], ds <: Kind.List] extends Deriving0_8[t, ds] with MonadTrans[t] {
        protected def deriveMonad[n[+_]](_N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L]
        protected def deriveMonadBase[n[+_], b[+_]](_N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = t[n, a]})#L] = defaultMonadBase(deriveMonad(_N), _N)
        protected def deriveMonadBase1[n[+_]](_N: Monad[n]): MonadBase[n, ({type L[+a] = t[n, a]})#L] = deriveMonadBase(MonadBase._ofSame(_N))
        protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = defaultMonadCont(deriveMonadBase1(_N), _N)
        protected def deriveMonadError[n[+_], e](_N: MonadError[e, n]): MonadError[e, ({type L[+a] = t[n, a]})#L] = defaultMonadError(deriveMonadBase1(_N), _N)
        protected def deriveMonadFix[n[+_]](_N: MonadFix[n]): MonadFix[({type L[+a] = t[n, a]})#L] = defaultMonadFix(deriveMonadBase1(_N), _N)
        protected def deriveMonadIO[n[+_]](_N: MonadIO[n]): MonadIO[({type L[+a] = t[n, a]})#L] = defaultMonadIO(deriveMonadBase1(_N), _N)
        protected def deriveMonadPlus[n[+_]](_N: MonadPlus[n]): MonadPlus[({type L[+a] = t[n, a]})#L] = defaultMonadPlus(deriveMonadBase1(_N), _N)
        protected def deriveMonadReader[n[+_], r](_N: MonadReader[r, n]): MonadReader[r, ({type L[+a] = t[n, a]})#L] = defaultMonadReader(deriveMonadBase1(_N), _N)
        protected def deriveMonadState[n[+_], s](_N: MonadState[s, n]): MonadState[s, ({type L[+a] = t[n, a]})#L] = defaultMonadState(deriveMonad(_N), _N)
    }

    private[ken] sealed trait Deriving0_0[t[_[+_], +_], ds <: Kind.List] { this: Deriving0[t, ds] =>
        final implicit def __asMonadTrans: MonadTrans[t] = this
        final implicit def __asMonad[n[+_]](implicit _On: Contains[ds, Monad.type], _N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L] = deriveMonad(_N)
    }
    private[ken] sealed trait Deriving0_1[t[_[+_], +_], ds <: Kind.List] extends Deriving0_0[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadBase[n[+_], b[+_]](implicit _On: Contains[ds, MonadBase.type], _N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = t[n, a]})#L] = deriveMonadBase(_N)
    }
    private[ken] sealed trait Deriving0_2[t[_[+_], +_], ds <: Kind.List] extends Deriving0_1[t, ds] { this: Deriving0[t, ds] =>
        // final implicit def __asMonadCont[n[+_]](implicit _On: Contains[ds, MonadCont.type], _N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = deriveMonadCont(_N)
    }
    private[ken] sealed trait Deriving0_3[t[_[+_], +_], ds <: Kind.List] extends Deriving0_2[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadError[n[+_], e](implicit _On: Contains[ds, MonadError.type], _N: MonadError[e, n]): MonadError[e, ({type L[+a] = t[n, a]})#L] = deriveMonadError(_N)
    }
    private[ken] sealed trait Deriving0_4[t[_[+_], +_], ds <: Kind.List] extends Deriving0_3[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadFix[n[+_]](implicit _On: Contains[ds, MonadFix.type], _N: MonadFix[n]): MonadFix[({type L[+a] = t[n, a]})#L] = deriveMonadFix(_N)
    }
    private[ken] sealed trait Deriving0_5[t[_[+_], +_], ds <: Kind.List] extends Deriving0_4[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadIO[n[+_]](implicit _On: Contains[ds, MonadIO.type], _N: MonadIO[n]): MonadIO[({type L[+a] = t[n, a]})#L] = deriveMonadIO(_N)
    }
    private[ken] sealed trait Deriving0_6[t[_[+_], +_], ds <: Kind.List] extends Deriving0_5[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadPlus[n[+_]](implicit _On: Contains[ds, MonadPlus.type], _N: MonadPlus[n]): MonadPlus[({type L[+a] = t[n, a]})#L] = deriveMonadPlus(_N)
    }
    private[ken] sealed trait Deriving0_7[t[_[+_], +_], ds <: Kind.List] extends Deriving0_6[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadReader[n[+_], r](implicit _On: Contains[ds, MonadReader.type], _N: MonadReader[r, n]): MonadReader[r, ({type L[+a] = t[n, a]})#L] = deriveMonadReader(_N)
    }
    private[ken] sealed trait Deriving0_8[t[_[+_], +_], ds <: Kind.List] extends Deriving0_7[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadState[n[+_], s](implicit _On: Contains[ds, MonadState.type], _N: MonadState[s, n]): MonadState[s, ({type L[+a] = t[n, a]})#L] = deriveMonadState(_N)
    }

    // One extra parameter
    //
    trait Deriving1[t1[_, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_8[t1, c, ds] {
        protected def deriveMonadTrans[x](_C: c[x]): MonadTrans[({type L[n[+_], +a] = t1[x, n, a]})#L]
        protected def deriveMonad[x, n[+_]](_N: Monad[n], _C: c[x]): Monad[({type L[+a] = t1[x, n, a]})#L]
        protected def deriveMonadBase[x, n[+_], b[+_]](_N: MonadBase[b, n], _C: c[x]): MonadBase[b, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadBase(deriveMonad(_N, _C), _N)
        protected def deriveMonadBase1[x, n[+_]](_N: Monad[n], _C: c[x]): MonadBase[n, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadBase(MonadBase._ofSame(_N), _C)
        protected def deriveMonadCont[x, n[+_]](_N: MonadCont[n], _C: c[x]): MonadCont[({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadCont(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadError[x, n[+_], e](_N: MonadError[e, n], _C: c[x]): MonadError[e, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadError(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadFix[x, n[+_]](_N: MonadFix[n], _C: c[x]): MonadFix[({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadFix(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadIO[x, n[+_]](_N: MonadIO[n], _C: c[x]): MonadIO[({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadIO(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadPlus[x, n[+_]](_N: MonadPlus[n], _C: c[x]): MonadPlus[({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadPlus(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadReader[x, n[+_], r](_N: MonadReader[r, n], _C: c[x]): MonadReader[r, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadReader(deriveMonadBase1(_N, _C), _N)
        protected def deriveMonadState[x, n[+_], s](_N: MonadState[s, n], _C: c[x]): MonadState[s, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadTrans(_C).defaultMonadState(deriveMonad(_N, _C), _N)
    }

    private[ken] sealed trait Deriving1_0[t1[x, _[+_], +_], c[_], ds <: Kind.List] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadTrans[x](implicit _C: c[x]): MonadTrans[({type L[n[+_], +a] = t1[x, n, a]})#L] = deriveMonadTrans(_C)
        final implicit def __asMonad[x, n[+_]](implicit _On: Contains[ds, Monad.type], _N: Monad[n], _C: c[x]): Monad[({type L[+a] = t1[x, n, a]})#L] = deriveMonad(_N, _C)
    }
    private[ken] sealed trait Deriving1_1[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_0[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadBase[x, n[+_], b[+_]](implicit _On: Contains[ds, MonadBase.type], _N: MonadBase[b, n], _C: c[x]): MonadBase[b, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadBase(_N, _C)
    }
    private[ken] sealed trait Deriving1_2[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_1[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        // final implicit def __asMonadCont[x, n[+_]](implicit _On: Contains[ds, MonadCont.type], _N: MonadCont[n], _C: c[x]): MonadCont[({type L[+a] = t1[x, n, a]})#L] = deriveMonadCont(_N, _C)
    }
    private[ken] sealed trait Deriving1_3[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_2[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadError[x, n[+_], e](implicit _On: Contains[ds, MonadError.type], _N: MonadError[e, n], _C: c[x]): MonadError[e, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadError(_N, _C)
    }
    private[ken] sealed trait Deriving1_4[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_3[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadFix[x, n[+_]](implicit _On: Contains[ds, MonadFix.type], _N: MonadFix[n], _C: c[x]): MonadFix[({type L[+a] = t1[x, n, a]})#L] = deriveMonadFix(_N, _C)
    }
    private[ken] sealed trait Deriving1_5[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_4[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadIO[x, n[+_]](implicit _On: Contains[ds, MonadIO.type], _N: MonadIO[n], _C: c[x]): MonadIO[({type L[+a] = t1[x, n, a]})#L] = deriveMonadIO(_N, _C)
    }
    private[ken] sealed trait Deriving1_6[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_5[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadPlus[x, n[+_]](implicit _On: Contains[ds, MonadPlus.type], _N: MonadPlus[n], _C: c[x]): MonadPlus[({type L[+a] = t1[x, n, a]})#L] = deriveMonadPlus(_N, _C)
    }
    private[ken] sealed trait Deriving1_7[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_6[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadReader[x, n[+_], r](implicit _On: Contains[ds, MonadReader.type], _N: MonadReader[r, n], _C: c[x]): MonadReader[r, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadReader(_N, _C)
    }
    private[ken] sealed trait Deriving1_8[t1[x, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_7[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadState[x, n[+_], s](implicit _On: Contains[ds, MonadState.type], _N: MonadState[s, n], _C: c[x]): MonadState[s, ({type L[+a] = t1[x, n, a]})#L] = deriveMonadState(_N, _C)
    }
}


sealed trait MonadTransInstance { this: MonadTrans.type =>
}
