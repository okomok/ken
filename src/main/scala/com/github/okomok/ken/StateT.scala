

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class StateT[s, n[+_], +a](override val old: s => n[(a, s)]) extends NewtypeOf[s => n[(a, s)]] with (s => n[(a, s)]) {
    override def apply(x: s): n[(a, s)] = old(x)
}


object StateT extends StateTOp with StateTAs with Kind.FunctionLike {
    trait apply[s] extends apply1[s]
    trait apply1[s] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = StateT[s, n, a]
    }

    trait apply2[s, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = StateT[s, n#apply1, a]
        override type oldtype1[+a] = s => n#apply1[(a, s)]
    }
}


private[ken] trait StateTOp {
    def run[s, n[+_], a](m: StateT[s, n, a]): s => n[(a, s)] = m.run

    def eval[s, n[+_], a](m: StateT[s, n, a])(s: s)(implicit i: Monad[n]): n[a] = {
        import i.`for`
        for { (a, _) <- run(m)(s) } yield a
    }

    def exec[s, n[+_], a](m: StateT[s, n, a])(s: s)(implicit i: Monad[n]): n[s] = {
        import i.`for`
        for { (_, s) <- run(m)(s) } yield s
    }

    def map[s, n[+_], u[+_], a, b](f: n[(a, s)] => u[(b, s)])(m: StateT[s, n, a]): StateT[s, u, b] = StateT { f `.` run(m) }

    def `with`[s, n[+_], a](f: s => s)(m: StateT[s, n, a]): StateT[s, n, a] = StateT { run(m) `.` f }
}


private[ken] sealed trait StateTAs0 { this: StateT.type =>
    implicit def _asNewtype1[s, n[+_]]: Newtype1[({type L[+a] = StateT[s, n, a]})#L, ({type L[+a] = s => n[(a, s)]})#L] = new Newtype1[({type L[+a] = StateT[s, n, a]})#L, ({type L[+a] = s => n[(a, s)]})#L] {
        private type nt[+a] = StateT[s, n, a]
        private type ot[+a] = s => n[(a, s)]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = StateT(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    }

    implicit def _asMonadTrans[s]: MonadTrans[({type L[n[+_], +a] = StateT[s, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = StateT[s, n, a]})#L] {
        private type t[n[+_], +a] = StateT[s, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = StateT { s => {
            import i.`for`
            for { a <- n } yield (a, s)
        } }
        override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = StateT { s =>
            i.liftM((x: a) => (x, s)) {
                f {
                    new Run {
                        override def apply[n_[+_], o[+_], b](t: t[n_, b], * : Type1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                            ri.liftM((x_s_ : (b, s)) => StateT((_: s) => rj.`return`(x_s_)))(run(t)(s))
                        }
                    }
                }
            }
        }
    }

    implicit def _asMonadPlus[s, n[+_]](implicit i: MonadPlus[n]): MonadPlus[({type L[+a] = StateT[s, n, a]})#L] = new MonadPlus[({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def mzero: m[Nothing] = StateT { _ => i.mzero }
        override def mplus[a](m1: m[a])(m2: Lazy[m[a]]): m[a] = StateT { s =>
            i.mplus(run(m1)(s))(run(m2)(s))
        }
    }

    implicit def _asMonadFix[s, n[+_]](implicit i: MonadFix[n]): MonadFix[({type L[+a] = StateT[s, n, a]})#L] = new MonadFix[({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = StateT { s =>
            i.mfix { (aI_ : Lazy[(a, s)]) => run(f(aI_._1))(s) }
        }
    }

    implicit def _asMonadCont[s, n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = StateT[s, n, a]})#L] = new MonadCont[({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
            i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                run( f( a => StateT { s_ => c((a, s_)) } ) )(s)
            }
        }
    }

    implicit def _asMonadError[s, n[+_], e](implicit i: MonadError[e, n]): MonadError[e, ({type L[+a] = StateT[s, n, a]})#L] = new MonadError[e, ({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = StateT { s =>
            i.catchError(run(m)(s)) { e => run(h(e))(s) }
        }
    }

    implicit def _asMonadReader[s, n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, ({type L[+a] = StateT[s, n, a]})#L] = new MonadReader[r, ({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = StateT { s => i.local(f)(run(m)(s)) }
    }

    implicit def _asMonadWriter[s, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = StateT[s, n, a]})#L] = new MonadWriter[w, ({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def monoid: Monoid[w] = i.monoid
        override val tell: w => m[Unit] = x => _asMonadTrans.lift(i.tell(x))
        override def listen[a](m: m[a]): m[(a, w)] = StateT { s => {
            import i.`for`
            for { ((a, s_), w) <- i.listen(run(m)(s)) } yield ((a, w), s_)
        } }
        override def pass[a](m: m[(a, w => w)]): m[a] = StateT { s =>
            import i.`for`
            i.pass {
                for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
            }
        }
    }

    implicit def _asMonadIO[s, n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = StateT[s, n, a]})#L] = new MonadIO[({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait StateTAs1 extends StateTAs0 { this: StateT.type =>
    implicit def _asMonadControlIO[s, n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[({type L[+a] = StateT[s, n, a]})#L] = new MonadControlIO[({type L[+a] = StateT[s, n, a]})#L] with MonadIOProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        private val mt = _asMonadTrans[s]
        override val selfMonadIO = _asMonadIO[s, n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
        override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
            mt.liftWith { run1 =>
                i.liftControlIO { runInBase =>
                    f {
                        new RunInIO {
                            override def apply[b](t: m[b]): IO[m[b]] = IO.liftM((x: n[m[b]]) => join(mt.lift(x)))(runInBase(run1(t)))
                        }
                    }
                }
            }
        }
    }
}

private[ken] sealed trait StateTAs extends StateTAs1 { this: StateT.type =>
    implicit def _asMonadState[s, n[+_]](implicit i: Monad[n]): MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] = new MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] {
        // Functor
        private type f[+a] = StateT[s, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => StateT { s =>
            import i.`for`
            for { (x, s_) <- run(m)(s) } yield (f(x), s_)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = StateT { s => i.`return`(a.!, s) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
            import i.`for`
            for { (a, s_) <- run(m)(s) } { run(k(a))(s_) }
        }
        // MonadState
        override val get: m[s] = StateT { s => i.`return`(s, s) }
        override val put: s => m[Unit] = s => StateT { _ => i.`return`((), s) }
    }
}
