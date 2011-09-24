

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


private[ken] final class _ListTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _ListT[+a](override val get: n[List[a]]) extends NewtypeOf[n[List[a]]]

    object _ListT extends _ListT_ with MonadPlus[_ListT] with Kind.MonadTrans {
        override type oldtype1[+a] = n[List[a]]
        override type innerMonad[+a] = n[a]

        implicit def dependent[a](n: NewtypeOf[n[List[a]]]): _ListT[a] = _ListT { n.run }

        def run[a](n: _ListT[a]): n[List[a]] = n.run

        def map[m[+_], a, b](f: n[List[a]] => m[List[b]])(n: _ListT[a]): NewtypeOf[m[List[b]]] = NewtypeOf { f(run(n)) }

        // Overrides
        //
        // Functor
        private type f[+a] = _ListT[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ListT {
            for { a <- run(m) } yield List.map(f)(a)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = _ListT { inner.`return`(List(a.!)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ListT {
            for { a <- run(m); b <- inner.mapM(run[b]_ `.` k)(a) } yield List.concat(b)
        }
        // MonadPlus
        override def mzero: m[Nothing] = _ListT { inner.`return`(Nil) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _ListT {
            for { a <- run(m); b <- run(n) } yield a ++: b
        }
    }

    private[ken] trait _ListT_0 { this: _ListT.type =>
        implicit val _asNewtype1: Newtype1[_ListT, ({type ot[+a] = n[List[a]]})#ot] = new Newtype1[_ListT, ({type ot[+a] = n[List[a]]})#ot] {
            private type nt[+a] = _ListT[a]
            private type ot[+a] = n[List[a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _ListT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit val _asMonadPlus: MonadPlus[_ListT] = this

        implicit val _asMonadTrans: MonadTrans[n, _ListT] = new MonadTrans[n, _ListT] {
            private type m[+a] = _ListT[a]
            override def lift[a](n: n[a]): m[a] = _ListT {
                for { a <- n } yield List(a)
            }
        }
    }

    private[ken] trait _ListT_1 extends _ListT_0 { this: _ListT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_ListT] = new MonadIO[_ListT] with MonadProxy[_ListT] {
            private type m[+a] = _ListT[a]
            override def selfMonad = _asMonadPlus
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _ListT_2 extends _ListT_1 { this: _ListT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_ListT] = new MonadCont[_ListT] with MonadProxy[_ListT] {
            private type m[+a] = _ListT[a]
            override val selfMonad = _asMonadPlus
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ListT {
                i.callCC { (c: List[a] => n[List[b]]) =>
                    run( f( a => _ListT { c(List(a)) } ) )
                }
            }
        }
    }

    private[ken] trait _ListT_3 extends _ListT_2 { this: _ListT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _ListT] = new MonadError[e, _ListT] with MonadProxy[_ListT] {
            private type m[+a] = _ListT[a]
            override val selfMonad = _asMonadPlus
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ListT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait _ListT_4 extends _ListT_3 { this: _ListT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _ListT] = new MonadReader[r, _ListT] with MonadProxy[_ListT] {
            private type m[+a] = _ListT[a]
            override val selfMonad = _asMonadPlus
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _ListT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait _ListT_ extends _ListT_4 { this: _ListT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _ListT] = new MonadState[s, _ListT] with MonadProxy[_ListT] {
            private type m[+a] = _ListT[a]
            override val selfMonad = _asMonadPlus
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }
}
