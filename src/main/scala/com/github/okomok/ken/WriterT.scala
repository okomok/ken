

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

    trait apply2[w, n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = WriterT[w, n, a]
        override type oldtype1[+a] = n[(a, w)]
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
    implicit def _asMonadTrans[w](implicit j: Monoid[w]): MonadTransControl[apply1[w]#monadTrans] = new MonadTransControl[apply1[w]#monadTrans] {
        // MonadTrans
        private type t[n[+_], +a] = WriterT[w, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = WriterT {
            import i.`for`
            for { a <- n } yield (a, j.mempty)
        }
        // MonadTransControl
        override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = WriterT {
            i.liftM((x: a) => (x, j.mempty)) {
                f {
                    new Run {
                        override def apply[n_[+_], o[+_], b](t: t[n_, b], * : TypeC1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                            ri.liftM((x_w: (b, w)) => WriterT(rj.`return`(x_w)))(run(t))
                        }
                    }
                }
            }
        }
    }

   implicit def _asMonadPlus[w, n[+_]](implicit i: MonadPlus[n], j: Monoid[w]): MonadPlus[apply2[w, n]#apply1] = new MonadPlus[apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def mzero: m[Nothing] = WriterT { i.mzero }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = WriterT { i.mplus(run(m))(run(n)) }
    }

    implicit def _asMonadFix[w, n[+_]](implicit i: MonadFix[n], j: Monoid[w]): MonadFix[apply2[w, n]#apply1] = new MonadFix[apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def mfix[a](m: Lazy[a] => m[a]): m[a] = WriterT {
            def k(aI_ : Lazy[(a, w)]) = run(m(aI_.!._1))
            i.mfix(k)
        }
    }

    implicit def _asMonadCont[w, n[+_]](implicit i: MonadCont[n], j: Monoid[w]): MonadCont[apply2[w, n]#apply1] = new MonadCont[apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = WriterT {
            i.callCC { (c: ((a, w)) => n[(b, w)]) =>
                run( f(a => WriterT { c((a, j.mempty)) }) )
            }
        }
    }

    implicit def _asMonadError[w, n[+_], e](implicit i: MonadError[e, n], j: Monoid[w]): MonadError[e, apply2[w, n]#apply1] = new MonadError[e, apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans[w].lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = WriterT {
            i.catchError(run(m)) { e =>
                run(h(e))
            }
        }
    }

    implicit def _asMonadReader[w, n[+_], r](implicit i: MonadReader[r, n], j: Monoid[w]): MonadReader[r, apply2[w, n]#apply1] = new MonadReader[r, apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def ask: m[r] = _asMonadTrans[w].lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = WriterT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[w, n[+_], s](implicit i: MonadState[s, n], j: Monoid[w]): MonadState[s, apply2[w, n]#apply1] = new MonadState[s, apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def get: m[s] = _asMonadTrans[w].lift(i.get)
        override def put(s: s): m[Unit] = _asMonadTrans[w].lift(i.put(s))
    }

    implicit def _asMonadIO[w, n[+_]](implicit i: MonadIO[n], j: Monoid[w]): MonadIO[apply2[w, n]#apply1] = new MonadIO[apply2[w, n]#apply1] with MonadProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        override val selfMonad = _asMonadWriter[w, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans[w].lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait WriterTAs1 extends WriterTAs0 { this: WriterT.type =>
    implicit def _asMonadControlIO[w, n[+_]](implicit i: MonadControlIO[n], j: Monoid[w]): MonadControlIO[apply2[w, n]#apply1] = new MonadControlIO[apply2[w, n]#apply1] with MonadIOProxy[apply2[w, n]#apply1] {
        private type m[+a] = WriterT[w, n, a]
        private val mt = _asMonadTrans[w]
        override val selfMonadIO = _asMonadIO[w, n]
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

private[ken] sealed trait WriterTAs extends WriterTAs1 { this: WriterT.type =>
    implicit def _asMonadWriter[w, n[+_]](implicit i: Monad[n], j: Monoid[w]): MonadWriter[w, apply2[w, n]#apply1] = new MonadWriter[w, apply2[w, n]#apply1] {
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
        override def tell(w: w): m[Unit] = WriterT { i.`return`((), w) }
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
