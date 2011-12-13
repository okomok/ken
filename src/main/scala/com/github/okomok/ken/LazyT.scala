

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


final case class LazyT[n[+_], +a](override val old: n[Lazy[a]]) extends NewtypeOf[n[Lazy[a]]]


object LazyT extends LazyTOp with LazyTAs with MonadTransControl[LazyT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = LazyT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[Lazy[a]]
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], +a] = LazyT[n, a]
    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = LazyT {
        import i.`for`
        for { a <- n } yield Lazy(a)
    }
    // MonadTransControl
    override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = LazyT {
         i.liftM((x: a) => Lazy.`return`(x)) {
            f {
                new Run {
                    override def apply[n_[+_], o[+_], b](t: t[n_, b], * : Type1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                        ri.liftM((x: Lazy[b]) => LazyT(rj.`return`(Lazy(x))))(run(t))
                    }
                }
            }
        }
    }
}


private[ken] trait LazyTOp {
    def run[n[+_], a](m: LazyT[n, a]): n[Lazy[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[Lazy[a]] => u[Lazy[b]])(m: LazyT[n, a]): LazyT[u, b] = LazyT { f(run(m)) }
}


private[ken] sealed trait LazyTAs0 { this: LazyT.type =>
    /*
    @Annotation.typeAliasWorkaround
    implicit def _asNewtype1[n[+_]]: Newtype1[({type L[+a] = LazyT[n, a]})#L, apply1[n]#oldtype1] = new Newtype1[({type L[+a] = LazyT[n, a]})#L, apply1[n]#oldtype1] {
        // private type nt[+a] = LazyT[n, a]
        // private type ot[+a] = n[Lazy[a]]
        override def newOf[a](ot: Lazy[n[Lazy[a]]]): LazyT[n, a] = LazyT(ot)
        override def oldOf[a](nt: Lazy[LazyT[n, a]]): n[Lazy[a]] = nt.run
    }
    */

    implicit val _asMonadTrans: MonadTransControl[LazyT] = this

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = LazyT[n, a]})#L] = new MonadCont[({type L[+a] = LazyT[n, a]})#L] with MonadProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        override val selfMonad = _asMonad[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = LazyT {
            i.callCC { (c: Lazy[a] => n[Lazy[b]]) =>
                run( f( a => LazyT { c(Lazy(a)) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, ({type L[+a] = LazyT[n, a]})#L] = new MonadError[e, ({type L[+a] = LazyT[n, a]})#L] with MonadProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        override val selfMonad = _asMonad[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = LazyT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, ({type L[+a] = LazyT[n, a]})#L] = new MonadReader[r, ({type L[+a] = LazyT[n, a]})#L] with MonadProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        override val selfMonad = _asMonad[n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = LazyT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, ({type L[+a] = LazyT[n, a]})#L] = new MonadState[s, ({type L[+a] = LazyT[n, a]})#L] with MonadProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        override val selfMonad = _asMonad[n]
        override val get: m[s] = _asMonadTrans.lift(i.get)
        override val put: s => m[Unit] = s => _asMonadTrans.lift(i.put(s))
    }

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = LazyT[n, a]})#L] = new MonadIO[({type L[+a] = LazyT[n, a]})#L] with MonadProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        override val selfMonad = _asMonad[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait LazyTAs1 extends LazyTAs0 { this: LazyT.type =>
    implicit def _asMonadControlIO[n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[({type L[+a] = LazyT[n, a]})#L] = new MonadControlIO[({type L[+a] = LazyT[n, a]})#L] with MonadIOProxy[({type L[+a] = LazyT[n, a]})#L] {
        private type m[+a] = LazyT[n, a]
        private val mt = _asMonadTrans
        override val selfMonadIO = _asMonadIO[n]
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

private[ken] sealed trait LazyTAs extends LazyTAs1 { this: LazyT.type =>
    implicit def _asMonad[n[+_]](implicit i: Monad[n]): Monad[({type L[+a] = LazyT[n, a]})#L] with HighPriority = new Monad[({type L[+a] = LazyT[n, a]})#L] with HighPriority {
        // Functor
        private type f[+a] = LazyT[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => LazyT {
            import i.`for`
            for { a <- run(m) } yield Lazy(f(a.!))
        }
        // Monad
        private type m[+a] = LazyT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = LazyT { i.`return`(Lazy(a)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = LazyT {
            import i.`for`
            for { a <- run(m) } { run(k(a.!)) }
        }
    }
}
