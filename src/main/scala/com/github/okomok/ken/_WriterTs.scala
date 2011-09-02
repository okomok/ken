

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


private[ken] final class _WriterTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _WriterT[w, +a](override val get: n[(a, w)]) extends NewtypeOf[n[(a, w)]]

    object _WriterT extends _WriterT_ with Kind.FunctionLike {
        sealed trait apply[w] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _WriterT[w, a]
            override type oldtype1[+a] = n[(a, w)]
            override type innerMonad[+a] = n[a]
        }

        implicit def dependent[w, a](n: NewtypeOf[n[(a, w)]]): _WriterT[w, a] = _WriterT(n.run)

        def run[w, a](n: _WriterT[w, a]): n[(a, w)] = n.run

        def exec[w, a](n: _WriterT[w, a]): n[w] = for { (_, w) <- run(n) } yield w

        def map[w, w_, m[+_], a, b](f: n[(a, w)] => m[(b, w_)])(n: _WriterT[w, a]): NewtypeOf[m[(b, w_)]] = NewtypeOf { f(run(n)) }
    }

    private[ken] trait _WriterT_0 { this: _WriterT.type =>
        implicit def _asNewtype1[w]: Newtype1[({type nt[+a] = _WriterT[w, a]})#nt, ({type ot[+a] = n[(a, w)]})#ot] = new Newtype1[({type nt[+a] = _WriterT[w, a]})#nt, ({type ot[+a] = n[(a, w)]})#ot] {
            private type nt[+a] = _WriterT[w, a]
            private type ot[+a] = n[(a, w)]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _WriterT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonadWriter[w](implicit i: Monoid[w]): MonadWriter[w, ({type m[+a] = _WriterT[w, a]})#m] = new MonadWriter[w, ({type m[+a] = _WriterT[w, a]})#m] {
            // Functor
            private type f[+a] = _WriterT[w, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _WriterT {
                for { (a, w) <- run(m) } yield (f(a), w)
            }
            // Monad
            private type m[+a] = f[a]
            override def `return`[a](a: Lazy[a]): m[a] = _WriterT { inner.`return`(a.!, i.mempty) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _WriterT {
                for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, i.mappend(w)(w_))
            }
            // MonadWriter
            override def monoid: Monoid[w] = i
            override def tell(w: w): m[Unit] = _WriterT { inner.`return`((), w) }
            override def listen[a](m: m[a]): m[(a, w)] = _WriterT {
                for { (a, w) <- run(m) } yield ((a, w), w)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = _WriterT {
                for { ((a, f), w) <- run(m) } yield (a, f(w))
            }
        }

        implicit def _asMonadTrans[w](implicit i: Monoid[w]): MonadTrans[n, ({type m[+a] = _WriterT[w, a]})#m] = new MonadTrans[n, ({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override def lift[a](n: n[a]): m[a] = _WriterT {
                for { a <- n } yield (a, i.mempty)
            }
        }
    }

    private[ken] trait _WriterT_1 extends _WriterT_0 { this: _WriterT.type =>
        implicit def _asMonadPlus[w](implicit i: MonadPlus[n], j: Monoid[w]): MonadPlus[({type m[+a] = _WriterT[w, a]})#m] = new MonadPlus[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def mzero: m[Nothing] = _WriterT { i.mzero }
            override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _WriterT { i.mplus(run(m))(run(n)) }
        }
    }

    private[ken] trait _WriterT_2 extends _WriterT_1 { this: _WriterT.type =>
        implicit def _asMonadFix[w](implicit i: MonadFix[n], j: Monoid[w]): MonadFix[({type m[+a] = _WriterT[w, a]})#m] = new MonadFix[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def mfix[a](m: Lazy[a] => m[a]): m[a] = _WriterT {
                def k(aI_ : Lazy[(a, w)]) = run(m(aI_.!._1))
                i.mfix(k)
            }
        }
    }

    private[ken] trait _WriterT_3 extends _WriterT_2 { this: _WriterT.type =>
        implicit def _asMonadIO[w](implicit i: MonadIO[n], j: Monoid[w]): MonadIO[({type m[+a] = _WriterT[w, a]})#m] = new MonadIO[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _WriterT_4 extends _WriterT_3 { this: _WriterT.type =>
        implicit def _asMonadCont[w](implicit i: MonadCont[n], j: Monoid[w]): MonadCont[({type m[+a] = _WriterT[w, a]})#m] = new MonadCont[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _WriterT {
                i.callCC { (c: ((a, w)) => n[(b, w)]) =>
                    run( f(a => _WriterT { c((a, j.mempty)) }) )
                }
            }
        }
    }

    private[ken] trait _WriterT_5 extends _WriterT_4 { this: _WriterT.type =>
        implicit def _asMonadError[w, e](implicit i: MonadError[e, n], j: Monoid[w]): MonadError[e, ({type m[+a] = _WriterT[w, a]})#m] = new MonadError[e, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _WriterT {
                i.catchError(run(m)) { e =>
                    run(h(e))
                }
            }
        }
    }

    private[ken] trait _WriterT_6 extends _WriterT_5 { this: _WriterT.type =>
        implicit def _asMonadReader[w, r](implicit i: MonadReader[r, n], j: Monoid[w]): MonadReader[r, ({type m[+a] = _WriterT[w, a]})#m] = new MonadReader[r, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _WriterT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait _WriterT_ extends _WriterT_6 { this: _WriterT.type =>
        implicit def _asMonadState[w, s](implicit i: MonadState[s, n], j: Monoid[w]): MonadState[s, ({type m[+a] = _WriterT[w, a]})#m] = new MonadState[s, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m] {
            private type m[+a] = _WriterT[w, a]
            override val selfMonad = _asMonadWriter[w]
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }
}
