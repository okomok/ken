

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


final case class WriterT[w, n[+_], +a](override val old: n[(a, w)]) extends NewtypeOf[n[(a, w)]]


object WriterT extends WriterTOp with WriterTAs with Kind.FunctionLike {
    trait apply1[w] extends Kind.MonadTrans {
    trait apply[w] extends apply1[w]
        override type monadTrans[n[+_], +a] = WriterT[w, n, a]
    }

    trait apply2[w, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = WriterT[w, n#apply1, a]
        override type oldtype1[+a] = n#apply1[(a, w)]
    }
}


private[ken] trait WriterTOp {
    def run[w, n[+_], a](n: WriterT[w, n, a]): n[(a, w)] = n.run

    def exec[w, n[+_], a](n: WriterT[w, n, a])(implicit i: Monad[n]): n[w] = {
        import i.`for`
        for { (_, w) <- run(n) } yield w
    }

    def map[w, w_, n[+_], u[+_], a, b](f: n[(a, w)] => u[(b, w_)])(m: WriterT[w, n, a]): WriterT[w_, u, b] = WriterT { f(run(m)) }
}


private[ken] sealed trait WriterTAs0 { this: WriterT.type =>
    implicit def _asMonadTrans[w](implicit _Mon: Monoid[w]): MonadTrans[({type L[n[+_], +a] = WriterT[w, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = WriterT[w, n, a]})#L] {
        private type t[n[+_], +a] = WriterT[w, n, a]
        final case class StT[+a](override val old: (a, w)) extends NewtypeOf[(a, w)]
        override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = WriterT {
            _N.liftM((x: a) => (x, _Mon.mempty)) {
                f {
                    new Run {
                        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                            _U.liftM((x: (b, w)) => StT(x))(run(t))
                        }
                    }
                }
            }
        }
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = WriterT {
            _N.liftM((St: StT[a]) => St.old)(nSt)
        }
    }

   implicit def _asMonadPlus[w, n[+_]](implicit i: MonadPlus[n], j: Monoid[w]): MonadPlus[({type L[+a] = WriterT[w, n, a]})#L] = new MonadPlus[({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def mzero: m[Nothing] = WriterT { i.mzero }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = WriterT { i.mplus(run(m))(run(n)) }
    }

    implicit def _asMonadFix[w, n[+_]](implicit i: MonadFix[n], j: Monoid[w]): MonadFix[({type L[+a] = WriterT[w, n, a]})#L] = new MonadFix[({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def mfix[a](m: Lazy[a] => m[a]): m[a] = WriterT {
            def k(aI_ : Lazy[(a, w)]) = run(m(aI_.!._1))
            i.mfix(k)
        }
    }

    implicit def _asMonadCont[w, n[+_]](implicit i: MonadCont[n], j: Monoid[w]): MonadCont[({type L[+a] = WriterT[w, n, a]})#L] = new MonadCont[({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = WriterT {
            i.callCC { (c: ((a, w)) => n[(b, w)]) =>
                run( f(a => WriterT { c((a, j.mempty)) }) )
            }
        }
    }

    implicit def _asMonadError[w, n[+_], e](implicit i: MonadError[e, n], j: Monoid[w]): MonadError[e, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadError[e, ({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans[w].lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = WriterT {
            i.catchError(run(m)) { e =>
                run(h(e))
            }
        }
    }

    implicit def _asMonadReader[w, n[+_], r](implicit i: MonadReader[r, n], j: Monoid[w]): MonadReader[r, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadReader[r, ({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def ask: m[r] = _asMonadTrans[w].lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = WriterT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[w, n[+_], s](implicit i: MonadState[s, n], j: Monoid[w]): MonadState[s, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadState[s, ({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override val get: m[s] = _asMonadTrans[w].lift(i.get)
        override val put: s => m[Unit] = s => _asMonadTrans[w].lift(i.put(s))
    }

    implicit def _asMonadIO[w, n[+_]](implicit i: MonadIO[n], j: Monoid[w]): MonadIO[({type L[+a] = WriterT[w, n, a]})#L] = new MonadIO[({type L[+a] = WriterT[w, n, a]})#L] with MonadProxy[({type L[+a] = WriterT[w, n, a]})#L] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[w].lift(i.liftIO(io))
    }

    implicit def _asMonadBase[w, n[+_], b[+_]](implicit _N: MonadBase[b, n], _Mon: Monoid[w]): MonadBase[b, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadBaseProxy[b, ({type L[+a] = WriterT[w, n, a]})#L] {
        type t[n[+_], +a] = WriterT[w, n, a]
        override val selfMonadBase = new MonadBase.TransDefault[t, n, b](_asMonadTrans[w], _N, _asMonadWriter(_N, _Mon))
    }
}

private[ken] sealed trait WriterTAs extends WriterTAs0 { this: WriterT.type =>
    implicit def _asMonadWriter[w, n[+_]](implicit i: Monad[n], j: Monoid[w]): MonadWriter[w, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadWriter[w, ({type L[+a] = WriterT[w, n, a]})#L] {
        // Functor
        private type f[+a] = WriterT[w, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => WriterT {
            import i.`for`
            for { (a, w) <- run(m) } yield (f(a), w)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = WriterT { i.`return`(a.!, j.mempty) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = WriterT {
            import i.`for`
            for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, j.mappend(w)(w_))
        }
        // MonadWriter
        override def monoid: Monoid[w] = j
        override val tell: w => m[Unit] = w => WriterT { i.`return`((), w) }
        override def listen[a](m: m[a]): m[(a, w)] = WriterT {
            import i.`for`
            for { (a, w) <- run(m) } yield ((a, w), w)
        }
        override def pass[a](m: m[(a, w => w)]): m[a] = WriterT {
            import i.`for`
            for { ((a, f), w) <- run(m) } yield (a, f(w))
        }
    }
}
