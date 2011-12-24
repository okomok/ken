

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010, Bas van Dijk, Anders Kaseorg
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadTransControl[t[_[+_], +_]] extends MonadTrans[t] { outer =>
    final val asMonadTransControl: MonadTransControl[monadTrans] = this

    type StT[+a]
    trait Run {
        def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]]
    }

    // Core
    //
    def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a]
    def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a]

    // Overrides
    //
    // MonadTrans
    override def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a] = liftWith(_ => n)(_N)

    // Default implementations
    //
    final def defaultMonadBaseControl[n[+_], b[+_]](_M: Monad[({type L[+a] = t[n, a]})#L], _N: MonadBaseControl[b, n]): MonadBaseControl[b, ({type L[+a] = t[n, a]})#L] = new MonadBaseControl[b, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        final case class StM[+a](override val old: _N.StM[outer.StT[a]]) extends NewtypeOf[_N.StM[outer.StT[a]]]
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override def baseMonad: baseMonad = _N.baseMonad
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

    final def defaultMonadCont[n[+_]](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = error("how?")
    }

    final def defaultMonadError[n[+_], e](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadError[e, n]): MonadError[e, ({type L[+a] = t[n, a]})#L] = new MonadError[e, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override val throwError: throwError = e => outer.lift(_N.throwError(e))(_N)
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _M.control { run =>
            _N.catchError(run(m)) { e => run(h(e)) }
        }
    }

    final def defaultMonadFix[n[+_]](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadFix[n]): MonadFix[({type L[+a] = t[n, a]})#L] = new MonadFix[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = _M.control { run =>
            val k: Lazy[_M.StM[a]] => n[_M.StM[a]] = st => {
                run(_M.op_>>=(_M.restoreM(st.!))(a => f(a)))
            }
            _N.mfix(k)
        }
    }

    final def defaultMonadPlus[n[+_]](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadPlus[n]): MonadPlus[({type L[+a] = t[n, a]})#L] = new MonadPlus[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override val mzero: mzero = outer.lift(_N.mzero)(_N)
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _M.control { run =>
            _N.mplus(run(m))(run(n.!))
        }
    }

    final def defaultMonadReader[n[+_], r](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadReader[r, n]): MonadReader[r, ({type L[+a] = t[n, a]})#L] = new MonadReader[r, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override def selfMonad: selfMonad = _M
        override val ask: m[r] = outer.lift(_N.ask)(_N)
        override def local[a](f: r => r)(m: m[a]): m[a] = _M.control { run =>
            _N.local(f)(run(m))
        }
    }

    final def defaultMonadWriter[n[+_], w](_M: MonadBaseControl[n, ({type L[+a] = t[n, a]})#L], _N: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = t[n, a]})#L] = new MonadWriter[w, ({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override val selfMonad: selfMonad = _M
        override def monoid: monoid = _N.monoid
        override val tell: tell = x => outer.lift(_N.tell(x))(_N)
        override def listen[a](m: m[a]): m[(a, w)] = error("how?")
        override def pass[a](m: m[(a, w => w)]): m[a] = error("how?")
    }
}


trait MonadTransControlProxy[t[_[+_], +_]] extends MonadTransControl[t] with MonadTransProxy[t] {
    type selfMonadTransControl = MonadTransControl[t]
    val selfMonadTransControl: selfMonadTransControl
    override def selfMonadTrans: selfMonadTrans = selfMonadTransControl
    override type StT[+a] = selfMonadTransControl.StT[a]

    private[this] def run2run(run: selfMonadTransControl.Run): Run = new Run {
        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = run(t)
    }


    override def lift[n[+_], a](n: n[a])(implicit _N: Monad[n]): t[n, a] = selfMonadTransControl.lift(n)
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = selfMonadTransControl.liftWith((run: selfMonadTransControl.Run) => f(run2run(run)))(_N)
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = selfMonadTransControl.restoreT(nSt)(_N)
}


object MonadTransControl extends MonadTransInstance {
    def apply[t <: Kind.MonadTrans](implicit _T: MonadTransControl[t#monadTrans]): MonadTransControl[t#monadTrans] = _T

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
    type AnyMonad = Monad.type ^: MonadBaseControl.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil

    import Kind.List.Contains

    // No extra parameters
    //
    trait Deriving0[t[_[+_], +_], ds <: Kind.List] extends Deriving0_9[t, ds] with MonadTransControl[t] {
        protected def deriveMonad[n[+_]](_N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L]
        protected def deriveMonadBaseControl[n[+_], b[+_]](_N: MonadBaseControl[b, n]): MonadBaseControl[b, ({type L[+a] = t[n, a]})#L] = defaultMonadBaseControl(deriveMonad(_N), _N)
        protected def deriveMonadBaseControl1[n[+_]](_N: Monad[n]): MonadBaseControl[n, ({type L[+a] = t[n, a]})#L] = deriveMonadBaseControl(MonadBase._ofSame(_N))
        protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = error("shall be overriden")
        protected def deriveMonadError[n[+_], e](_N: MonadError[e, n]): MonadError[e, ({type L[+a] = t[n, a]})#L] = defaultMonadError(deriveMonadBaseControl1(_N), _N)
        protected def deriveMonadFix[n[+_]](_N: MonadFix[n]): MonadFix[({type L[+a] = t[n, a]})#L] = defaultMonadFix(deriveMonadBaseControl1(_N), _N)
        protected def deriveMonadIO[n[+_]](_N: MonadIO[n]): MonadIO[({type L[+a] = t[n, a]})#L] = defaultMonadIO(deriveMonadBaseControl1(_N), _N)
        protected def deriveMonadPlus[n[+_]](_N: MonadPlus[n]): MonadPlus[({type L[+a] = t[n, a]})#L] = defaultMonadPlus(deriveMonadBaseControl1(_N), _N)
        protected def deriveMonadReader[n[+_], r](_N: MonadReader[r, n]): MonadReader[r, ({type L[+a] = t[n, a]})#L] = defaultMonadReader(deriveMonadBaseControl1(_N), _N)
        protected def deriveMonadState[n[+_], s](_N: MonadState[s, n]): MonadState[s, ({type L[+a] = t[n, a]})#L] = defaultMonadState(deriveMonad(_N), _N)
        protected def deriveMonadWriter[n[+_], w](_N: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = t[n, a]})#L] = error("shall be overriden")
    }

    private[ken] sealed trait Deriving0_0[t[_[+_], +_], ds <: Kind.List] { this: Deriving0[t, ds] =>
        final implicit def __asMonadTransControl: MonadTransControl[t] = this
        final implicit def __asMonad[n[+_]](implicit _On: Contains[ds, Monad.type], _N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L] = deriveMonad(_N)
    }
    private[ken] sealed trait Deriving0_1[t[_[+_], +_], ds <: Kind.List] extends Deriving0_0[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadBaseControl[n[+_], b[+_]](implicit _On: Contains[ds, MonadBaseControl.type], _N: MonadBaseControl[b, n]): MonadBaseControl[b, ({type L[+a] = t[n, a]})#L] = deriveMonadBaseControl(_N)
    }
    private[ken] sealed trait Deriving0_2[t[_[+_], +_], ds <: Kind.List] extends Deriving0_1[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadCont[n[+_]](implicit _On: Contains[ds, MonadCont.type], _N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = deriveMonadCont(_N)
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
    private[ken] sealed trait Deriving0_9[t[_[+_], +_], ds <: Kind.List] extends Deriving0_8[t, ds] { this: Deriving0[t, ds] =>
        final implicit def __asMonadWriter[n[+_], w](implicit _On: Contains[ds, MonadWriter.type], _N: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = t[n, a]})#L] = deriveMonadWriter(_N)
    }

    // One extra parameter
    //
    trait Deriving1[t1[_, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_9[t1, c, ds] {
        protected def asMonadTransControl[z](_C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L]
        protected def deriveMonad[z, n[+_]](_N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L]
        protected def deriveMonadBaseControl[z, n[+_], b[+_]](_N: MonadBaseControl[b, n], _C: c[z]): MonadBaseControl[b, ({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadBaseControl(deriveMonad(_N, _C), _N)
        protected def deriveMonadBaseControl1[z, n[+_]](_N: Monad[n], _C: c[z]): MonadBaseControl[n, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadBaseControl(MonadBase._ofSame(_N), _C)
        protected def deriveMonadCont[z, n[+_]](_N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = error("shall be overriden")
        protected def deriveMonadError[z, n[+_], e](_N: MonadError[e, n], _C: c[z]): MonadError[e, ({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadError(deriveMonadBaseControl1(_N, _C), _N)
        protected def deriveMonadFix[z, n[+_]](_N: MonadFix[n], _C: c[z]): MonadFix[({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadFix(deriveMonadBaseControl1(_N, _C), _N)
        protected def deriveMonadIO[z, n[+_]](_N: MonadIO[n], _C: c[z]): MonadIO[({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadIO(deriveMonadBaseControl1(_N, _C), _N)
        protected def deriveMonadPlus[z, n[+_]](_N: MonadPlus[n], _C: c[z]): MonadPlus[({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadPlus(deriveMonadBaseControl1(_N, _C), _N)
        protected def deriveMonadReader[z, n[+_], r](_N: MonadReader[r, n], _C: c[z]): MonadReader[r, ({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadReader(deriveMonadBaseControl1(_N, _C), _N)
        protected def deriveMonadState[z, n[+_], s](_N: MonadState[s, n], _C: c[z]): MonadState[s, ({type L[+a] = t1[z, n, a]})#L] = asMonadTransControl(_C).defaultMonadState(deriveMonad(_N, _C), _N)
        protected def deriveMonadWriter[z, n[+_], w](_N: MonadWriter[w, n], _C: c[z]): MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] = error("shall be overriden")
    }

    private[ken] sealed trait Deriving1_0[t1[z, _[+_], +_], c[_], ds <: Kind.List] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadTransControl[z](implicit _C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] = asMonadTransControl(_C)
        final implicit def __asMonad[z, n[+_]](implicit _On: Contains[ds, Monad.type], _N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L] = deriveMonad(_N, _C)
    }
    private[ken] sealed trait Deriving1_1[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_0[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadBaseControl[z, n[+_], b[+_]](implicit _On: Contains[ds, MonadBaseControl.type], _N: MonadBaseControl[b, n], _C: c[z]): MonadBaseControl[b, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadBaseControl(_N, _C)
    }
    private[ken] sealed trait Deriving1_2[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_1[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadCont[z, n[+_]](implicit _On: Contains[ds, MonadCont.type], _N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = deriveMonadCont(_N, _C)
    }
    private[ken] sealed trait Deriving1_3[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_2[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadError[z, n[+_], e](implicit _On: Contains[ds, MonadError.type], _N: MonadError[e, n], _C: c[z]): MonadError[e, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadError(_N, _C)
    }
    private[ken] sealed trait Deriving1_4[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_3[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadFix[z, n[+_]](implicit _On: Contains[ds, MonadFix.type], _N: MonadFix[n], _C: c[z]): MonadFix[({type L[+a] = t1[z, n, a]})#L] = deriveMonadFix(_N, _C)
    }
    private[ken] sealed trait Deriving1_5[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_4[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadIO[z, n[+_]](implicit _On: Contains[ds, MonadIO.type], _N: MonadIO[n], _C: c[z]): MonadIO[({type L[+a] = t1[z, n, a]})#L] = deriveMonadIO(_N, _C)
    }
    private[ken] sealed trait Deriving1_6[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_5[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadPlus[z, n[+_]](implicit _On: Contains[ds, MonadPlus.type], _N: MonadPlus[n], _C: c[z]): MonadPlus[({type L[+a] = t1[z, n, a]})#L] = deriveMonadPlus(_N, _C)
    }
    private[ken] sealed trait Deriving1_7[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_6[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadReader[z, n[+_], r](implicit _On: Contains[ds, MonadReader.type], _N: MonadReader[r, n], _C: c[z]): MonadReader[r, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadReader(_N, _C)
    }
    private[ken] sealed trait Deriving1_8[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_7[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadState[z, n[+_], s](implicit _On: Contains[ds, MonadState.type], _N: MonadState[s, n], _C: c[z]): MonadState[s, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadState(_N, _C)
    }
    private[ken] sealed trait Deriving1_9[t1[z, _[+_], +_], c[_], ds <: Kind.List] extends Deriving1_8[t1, c, ds] { this: Deriving1[t1, c, ds] =>
        final implicit def __asMonadWriter[z, n[+_], w](implicit _On: Contains[ds, MonadWriter.type], _N: MonadWriter[w, n], _C: c[z]): MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] = deriveMonadWriter(_N, _C)
    }
}


sealed trait MonadTransControlInstance extends { this: MonadTransControl.type =>
}
