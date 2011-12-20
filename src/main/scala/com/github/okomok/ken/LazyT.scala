

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


object LazyT extends LazyTOp with LazyTAs with MonadTrans[LazyT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = LazyT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[Lazy[a]]
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], +a] = LazyT[n, a]
    final case class StT[+a](override val old: Lazy[a]) extends NewtypeOf[Lazy[a]]
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = LazyT {
         _N.liftM((a: a) => Lazy.`return`(a)) {
            f {
                new Run {
                    override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                        _U.liftM((x: Lazy[b]) => StT(x))(run(t))
                    }
                }
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = LazyT {
        _N.liftM((St: StT[a]) => St.old)(nSt)
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

    implicit val _asMonadTrans: MonadTrans[LazyT] = this

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

    implicit def _asMonadBase[n[+_], b[+_]](implicit _N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = LazyT[n, a]})#L] = new MonadBaseProxy[b, ({type L[+a] = LazyT[n, a]})#L] {
        type t[n[+_], +a] = LazyT[n, a]
        override val selfMonadBase = new MonadBase.TransDefault[t, n, b](_asMonadTrans, _N, _asMonad(_N))
    }
}

private[ken] sealed trait LazyTAs extends LazyTAs0 { this: LazyT.type =>
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
