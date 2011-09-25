

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


private[ken] final class _ReaderTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _ReaderT[r, +a](override val get: r => n[a]) extends NewtypeOf[r => n[a]]

    object _ReaderT extends _ReaderT_ with Kind.FunctionLike {
        sealed trait apply[r] extends Kind.MonadTransControl {
            override type apply1[+a] = _ReaderT[r, a]
            override type oldtype1[+a] = r => n[a]
            override type innerMonad[+a] = n[a]
            override type baseResult[+a] = a
        }

        implicit def dependent[r, a](n: NewtypeOf[r => n[a]]): _ReaderT[r, a] = _ReaderT { n.run }

        def run[r, a](n: _ReaderT[r, a]): r => n[a] = n.run

        def map[r, m[+_], a, b](f: n[a] => m[b])(n: _ReaderT[r, a]): NewtypeOf[r => m[b]] = NewtypeOf { f `.` run(n) }

        def `with`[r, r_, a](f: r_ => r)(n: _ReaderT[r, a]): _ReaderT[r_, a] = _ReaderT { run(n) `.` f }
    }

    private[ken] trait _ReaderT_0 { this: _ReaderT.type =>
        implicit def _asNewtype1[r]: Newtype1[({type nt[+a] = _ReaderT[r, a]})#nt, ({type ot[+a] = r => n[a]})#ot] = new Newtype1[({type nt[+a] = _ReaderT[r, a]})#nt, ({type ot[+a] = r => n[a]})#ot] {
            private type nt[+a] = _ReaderT[r, a]
            private type ot[+a] = r => n[a]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _ReaderT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonadReader[r]: MonadReader[r, ({type m[+a] = _ReaderT[r, a]})#m] = new MonadReader[r, ({type m[+a] = _ReaderT[r, a]})#m] {
            // Functor
            private type f[+a] = _ReaderT[r, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ReaderT { r =>
                for { a <- run(m)(r) } yield f(a)
            }
            // Monad
            private type m[+a] = f[a]
            override def `return`[a](a: Lazy[a]): m[a] = _ReaderT { r => inner.`return`(a) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ReaderT { r =>
                for { a <- run(m)(r); * <- run(k(a))(r) } yield *
            }
            // MonadReader
            override def ask: m[r] = _ReaderT { r => inner.`return`(r) }
            override def local[a](f: r => r)(m: m[a]): m[a] = _ReaderT { r => run(m)(f(r)) }
        }

        implicit def _asMonadTrans[r]: MonadTransControl[n, ({type m[+a] = _ReaderT[r, a]})#m, ({type u[+a] = a})#u] = new MonadTransControl[n, ({type m[+a] = _ReaderT[r, a]})#m, ({type u[+a] = a})#u] {
            // MonadTrans
            type m[+a] = _ReaderT[r, a]
            override def lift[a](n: n[a]): m[a] = _ReaderT { _ => n }
            // MonadTransControl
            type u[+a] = a
            override def liftControl[a](f: Run => n[a]): m[a] = _ReaderT { r =>
                f {
                    new Run {
                        override def apply[o[+_], b](t: m[b])(implicit i: Monad[o]): n[o[u[b]]] = {
                            inner.liftM((x: b) => i.`return`(x))(run(t)(r))
                        }
                    }
                }
            }
        }
    }

    private[ken] trait _ReaderT_1 extends _ReaderT_0 { this: _ReaderT.type =>
        implicit def _asMonadPlus[r](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = _ReaderT[r, a]})#m] = new MonadPlus[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def mzero: m[Nothing] = _ReaderT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _ReaderT { r => i.mplus(run(m)(r))(run(n)(r)) }
        }
    }

    private[ken] trait _ReaderT_2 extends _ReaderT_1 { this: _ReaderT.type =>
        implicit def _asMonadFix[r](implicit i: MonadFix[n]): MonadFix[({type m[+a] = _ReaderT[r, a]})#m] = new MonadFix[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def mfix[a](f: Lazy[a] => m[a]): m[a] = _ReaderT { r =>
                def k(a: Lazy[a]) = run(f(a))(r)
                i.mfix(k)
            }
        }
    }

    private[ken] trait _ReaderT_3 extends _ReaderT_2 { this: _ReaderT.type =>
        implicit def _asMonadIO[r](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _ReaderT[r, a]})#m] = new MonadIO[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            private val mt = _asMonadTrans[r]
            override val selfMonad = _asMonadReader[r]
            override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
        }
    }

    private[ken] trait _ReaderT_4 extends _ReaderT_3 { this: _ReaderT.type =>
        implicit def _asMonadCont[r](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _ReaderT[r, a]})#m] = new MonadCont[({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ReaderT { r =>
                i.callCC { (c: a => n[b]) =>
                    run( f( a => _ReaderT { s_ => c(a) } ) )(r)
                }
            }
        }
    }

    private[ken] trait _ReaderT_5 extends _ReaderT_4 { this: _ReaderT.type =>
        implicit def _asMonadError[r, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = _ReaderT[r, a]})#m] = new MonadError[e, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ReaderT { r =>
                i.catchError(run(m)(r)) { e => run(h(e))(r) }
            }
        }
    }

    private[ken] trait _ReaderT_6 extends _ReaderT_5 { this: _ReaderT.type =>
        implicit def _asMonadState[r, s](implicit i: MonadState[s, n]): MonadState[s, ({type m[+a] = _ReaderT[r, a]})#m] = new MonadState[s, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }

    private[ken] trait _ReaderT_7 extends _ReaderT_6 { this: _ReaderT.type =>
        implicit def _asMonadWriter[r, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = _ReaderT[r, a]})#m] = new MonadWriter[w, ({type m[+a] = _ReaderT[r, a]})#m] with MonadProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            override val selfMonad = _asMonadReader[r]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = _ReaderT { w => i.listen(run(m)(w)) }
            override def pass[a](m: m[(a, w => w)]): m[a] = _ReaderT { w => i.pass(run(m)(w)) }
        }
    }

    private[ken] trait _ReaderT_ extends _ReaderT_7 { this: _ReaderT.type =>
        implicit def _asMonadControlIO[r](implicit i: MonadControlIO[n]): MonadControlIO[({type m[+a] = _ReaderT[r, a]})#m] = new MonadControlIO[({type m[+a] = _ReaderT[r, a]})#m] with MonadIOProxy[({type m[+a] = _ReaderT[r, a]})#m] {
            private type m[+a] = _ReaderT[r, a]
            private val mt = _asMonadTrans[r]
            override val selfMonadIO = _asMonadIO[r](i)
            override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
            override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
                def dep[b](x: n[n[b]]): n[m[b]] = for { n <- x } yield _ReaderT((r: r) => n)
                mt.liftControl { run1 =>
                    i.liftControlIO { runInBase =>
                        f {
                            new RunInIO {
                                override def apply[b](t: m[b]): IO[m[b]] = IO.liftM((x: n[m[b]]) => join(mt.lift(x)))(runInBase(dep(run1[n, b](t))))
                            }
                        }
                    }
                }
            }
        }
    }
}
