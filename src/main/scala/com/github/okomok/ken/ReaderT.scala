

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


final case class ReaderT[r, n[+_], +a](override val old: r => n[a]) extends NewtypeOf[r => n[a]] with (r => n[a]) {
    override def apply(x: r): n[a] = old(x)
}


object ReaderT extends ReaderTOp with ReaderTAs with Kind.FunctionLike {
    trait apply[r] extends apply1[r]
    trait apply1[r] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = ReaderT[r, n, a]
    }

    trait apply2[r, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = ReaderT[r, n#apply1, a]
        override type oldtype1[+a] = r => n#apply1[a]
    }
}


private[ken] trait ReaderTOp {
    def run[r, n[+_], a](m: ReaderT[r, n, a]): r => n[a] = m.run

    def map[r, n[+_], u[+_], a, b](f: n[a] => u[b])(m: ReaderT[r, n, a]): ReaderT[r, u, b] = ReaderT { f `.` run(m) }

    def `with`[r, r_, n[+_], a](f: r_ => r)(m: ReaderT[r, n, a]): ReaderT[r_, n, a] = ReaderT { run(m) `.` f }
}


private[ken] sealed trait ReaderTAs0 { this: ReaderT.type =>
    implicit def _asMonadTrans[r]: MonadTransControl[({type L[n[+_], +a] = ReaderT[r, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = ReaderT[r, n, a]})#L] {
        // MonadTrans
        private type t[n[+_], +a] = ReaderT[r, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = ReaderT { _ => n }
        // MonadTransControl
        override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = ReaderT { r =>
            f {
                new Run {
                    override def apply[n_[+_], o[+_], b](t: t[n_, b], * : Type1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                        ri.liftM((x: b) => rk.`return`(x))(run(t)(r))
                    }
                }
            }
        }
    }

    implicit def _asMonadPlus[r, n[+_]](implicit i: MonadPlus[n]): MonadPlus[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadPlus[({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = _asMonadReader[r, n]
        override def mzero: m[Nothing] = ReaderT { _ => i.mzero }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = ReaderT { r => i.mplus(run(m)(r))(run(n)(r)) }
    }

    implicit def _asMonadFix[r, n[+_]](implicit i: MonadFix[n]): MonadFix[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadFix[({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = _asMonadReader[r, n]
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = ReaderT { r =>
            def k(a: Lazy[a]) = run(f(a))(r)
            i.mfix(k)
        }
    }

    implicit def _asMonadCont[r, n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadCont[({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = _asMonadReader[r, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ReaderT { r =>
            i.callCC { (c: a => n[b]) =>
                run( f( a => ReaderT { s_ => c(a) } ) )(r)
            }
        }
    }

    implicit def _asMonadError[r, n[+_], e](implicit i: MonadError[e, n]): MonadError[e, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadError[e, ({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = _asMonadReader[r, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans[r].lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = ReaderT { r =>
            i.catchError(run(m)(r)) { e => run(h(e))(r) }
        }
    }

    implicit def _asMonadState[r, n[+_], s](implicit i: MonadState[s, n]): MonadState[s, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadState[s, ({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        private val mt = _asMonadTrans[r]
        override val selfMonad = _asMonadReader[r, n]
        override val get: m[s] = mt.lift(i.get)
        override val put: s => m[Unit] = s => mt.lift(i.put(s))
    }

    implicit def _asMonadWriter[r, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadWriter[w, ({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = _asMonadReader[r, n]
        override def monoid: Monoid[w] = i.monoid
        override val tell: w => m[Unit] = x => _asMonadTrans[r].lift(i.tell(x))
        override def listen[a](m: m[a]): m[(a, w)] = ReaderT { w => i.listen(run(m)(w)) }
        override def pass[a](m: m[(a, w => w)]): m[a] = ReaderT { w => i.pass(run(m)(w)) }
    }

    implicit def _asMonadIO[r, n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadIO[({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        private val mt = _asMonadTrans[r]
        override val selfMonad = _asMonadReader[r, n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait ReaderTAs1 extends ReaderTAs0 { this: ReaderT.type =>
    implicit def _asMonadControlIO[r, n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadControlIO[({type L[+a] = ReaderT[r, n, a]})#L] with MonadIOProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        private val mt = _asMonadTrans[r]
        override val selfMonadIO = _asMonadIO[r, n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
        override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
            mt.liftControl { run1 =>
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

private[ken] sealed trait ReaderTAs extends ReaderTAs1 { this: ReaderT.type =>
    implicit def _asMonadReader[r, n[+_]](implicit i: Monad[n]): MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] {
        // Functor
        private type f[+a] = ReaderT[r, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ReaderT { r => {
            import i.`for`
            for { a <- run(m)(r) } yield f(a)
        } }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ReaderT { r => i.`return`(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ReaderT { r => {
            import i.`for`
            for { a <- run(m)(r) } { run(k(a))(r) }
        } }
        // MonadReader
        override def ask: m[r] = ReaderT { r => i.`return`(r) }
        override def local[a](f: r => r)(m: m[a]): m[a] = ReaderT { r => run(m)(f(r)) }
    }
}
