

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


private[ken] final class _StateTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerForComp[a](x: n[a]): inner.ForComp[a] = inner.forComp(x)

    final case class _StateT[s, +a](override val get: s => n[(a, s)]) extends NewtypeOf[s => n[(a, s)]]

    object _StateT extends _StateT_ with Kind.FunctionLike {
        sealed trait apply[s] extends Kind.AbstractMonadTrans {
            override type apply1[+a] = _StateT[s, a]
            override type oldtype1[+a] = s => n[(a, s)]
            override type innerMonad[+a] = n[a]
        }

        implicit def dependent[s, a](n: NewtypeOf[s => n[(a, s)]]): _StateT[s, a] = _StateT { n.run }

        def run[s, a](n: _StateT[s, a]): s => n[(a, s)] = n.run

        def eval[s, a](n: _StateT[s, a]): s => n[a] = s => for { (a, _) <- run(n)(s) } yield a

        def exec[s, a](n: _StateT[s, a]): s => n[s] = s => for { (_, s) <- run(n)(s) } yield s

        def map[s, m[+_], a, b](f: n[(a, s)] => m[(b, s)])(n: _StateT[s, a]): NewtypeOf[s => m[(b, s)]] = NewtypeOf { f compose run(n) }

        def `with`[s, a](f: s => s)(n: _StateT[s, a]): _StateT[s, a] = _StateT { run(n) compose f }
    }

    private[ken] trait _StateT_0 { this: _StateT.type =>
        implicit def _asNewtype1[s]: Newtype1[({type nt[+a] = _StateT[s, a]})#nt, ({type ot[+a] = s => n[(a, s)]})#ot] = new Newtype1[({type nt[+a] = _StateT[s, a]})#nt, ({type ot[+a] = s => n[(a, s)]})#ot] {
            private[this] type nt[+a] = _StateT[s, a]
            private[this] type ot[+a] = s => n[(a, s)]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _StateT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonadState[s]: MonadState[s, ({type m[+a] = _StateT[s, a]})#m] = new MonadState[s, ({type m[+a] = _StateT[s, a]})#m] {
            // Functor
            private[this] type f[+a] = _StateT[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _StateT { s =>
                for { (x, s_) <- run(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: Lazy[a]): m[a] = _StateT { s => inner.`return`(a.!, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _StateT { s =>
                for { (a, s_) <- run(m)(s); * <- run(k(a))(s_) } yield *
            }
            // MonadState
            override def get: m[s] = _StateT { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = _StateT { _ => inner.`return`((), s) }
        }

        implicit def _asMonadTrans[s]: MonadTrans[n, ({type m[+a] = _StateT[s, a]})#m] = new MonadTrans[n, ({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override def lift[a](n: n[a]): m[a] = _StateT { s => for { a <- n } yield (a, s) }
        }
    }

    private[ken] trait _StateT_1 extends _StateT_0 { this: _StateT.type =>
        implicit def _asMonadPlus[s](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = _StateT[s, a]})#m] = new MonadPlus[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def mzero: m[Nothing] = _StateT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _StateT { s => i.mplus(run(m)(s))(run(n)(s)) }
        }
    }

    private[ken] trait _StateT_2 extends _StateT_1 { this: _StateT.type =>
        implicit def _asMonadFix[s](implicit i: MonadFix[n]): MonadFix[({type m[+a] = _StateT[s, a]})#m] = new MonadFix[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def mfix[a](f: Lazy[a] => m[a]): m[a] = _StateT { s =>
                i.mfix { (aI_ : Lazy[(a, s)]) => run(f(aI_._1))(s) }
            }
        }
    }

    private[ken] trait _StateT_3 extends _StateT_2 { this: _StateT.type =>
        implicit def _asMonadIO[s](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _StateT[s, a]})#m] = new MonadIO[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _StateT_4 extends _StateT_3 { this: _StateT.type =>
        implicit def _asMonadCont[s](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _StateT[s, a]})#m] = new MonadCont[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _StateT { s =>
                i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                    run( f( a => _StateT { s_ => c((a, s_)) } ) )(s)
                }
            }
        }
    }

    private[ken] trait _StateT_5 extends _StateT_4 { this: _StateT.type =>
        implicit def _asMonadError[s, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = _StateT[s, a]})#m] = new MonadError[e, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _StateT { s =>
                i.catchError(run(m)(s)) { e => run(h(e))(s) }
            }
        }
    }

    private[ken] trait _StateT_6 extends _StateT_5 { this: _StateT.type =>
        implicit def _asMonadReader[s, r](implicit i: MonadReader[r, n]): MonadReader[r, ({type m[+a] = _StateT[s, a]})#m] = new MonadReader[r, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _StateT { s => i.local(f)(run(m)(s)) }
        }
    }

    private[ken] trait _StateT_ extends _StateT_6 { this: _StateT.type =>
        implicit def _asMonadWriter[s, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = _StateT[s, a]})#m] = new MonadWriter[w, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override val selfMonad = _asMonadState[s]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = _StateT { s =>
                for { ((a, s_), w) <- i.listen(run(m)(s)) } yield ((a, w), s_)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = _StateT { s =>
                i.pass {
                    for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
                }
            }
        }
    }
}
