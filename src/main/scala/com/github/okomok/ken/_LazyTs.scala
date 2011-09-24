

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


private[ken] final class _LazyTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _LazyT[+a](override val get: n[Lazy[a]]) extends NewtypeOf[n[Lazy[a]]]

    object _LazyT extends _LazyT_ with Kind.MonadTrans {
        override type apply1[+a] = _LazyT[a]
        override type oldtype1[+a] = n[Lazy[a]]
        override type innerMonad[+a] = n[a]

        implicit def dependent[a](n: NewtypeOf[n[Lazy[a]]]): _LazyT[a] = _LazyT { n.run }

        def run[a](n: _LazyT[a]): n[Lazy[a]] = n.run

        def map[m[+_], a, b](f: n[Lazy[a]] => m[Lazy[b]])(n: _LazyT[a]): NewtypeOf[m[Lazy[b]]] = NewtypeOf { f(run(n)) }
    }

    private[ken] trait _LazyT_0 { this: _LazyT.type =>
        implicit val _asNewtype1: Newtype1[_LazyT, ({type ot[+a] = n[Lazy[a]]})#ot] = new Newtype1[_LazyT, ({type ot[+a] = n[Lazy[a]]})#ot] {
            private type nt[+a] = _LazyT[a]
            private type ot[+a] = n[Lazy[a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _LazyT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit val _asMonad: Monad[_LazyT] = new Monad[_LazyT] {
            // Functor
            private type f[+a] = _LazyT[a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _LazyT {
                for { a <- run(m) } yield Lazy(f(a.!))
            }
            // Monad
            private type m[+a] = f[a]
            override def `return`[a](a: Lazy[a]): m[a] = _LazyT { inner.`return`(Lazy(a)) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _LazyT {
                for { a <- run(m); * <- run(k(a.!)) } yield *
            }
        }

        implicit val _asMonadTrans: MonadTrans[n, _LazyT] = new MonadTrans[n, _LazyT] {
            private type m[+a] = _LazyT[a]
            override def lift[a](n: n[a]): m[a] = _LazyT {
                for { a <- n } yield Lazy(a)
            }
        }
    }

    private[ken] trait _LazyT_1 extends _LazyT_0 { this: _LazyT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_LazyT] = new MonadIO[_LazyT] with MonadProxy[_LazyT] {
            private type m[+a] = _LazyT[a]
            override def selfMonad = _asMonad
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _LazyT_2 extends _LazyT_1 { this: _LazyT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_LazyT] = new MonadCont[_LazyT] with MonadProxy[_LazyT] {
            private type m[+a] = _LazyT[a]
            override val selfMonad = _asMonad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _LazyT {
                i.callCC { (c: Lazy[a] => n[Lazy[b]]) =>
                    run( f( a => _LazyT { c(Lazy(a)) } ) )
                }
            }
        }
    }

    private[ken] trait _LazyT_3 extends _LazyT_2 { this: _LazyT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _LazyT] = new MonadError[e, _LazyT] with MonadProxy[_LazyT] {
            private type m[+a] = _LazyT[a]
            override val selfMonad = _asMonad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _LazyT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait _LazyT_4 extends _LazyT_3 { this: _LazyT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _LazyT] = new MonadReader[r, _LazyT] with MonadProxy[_LazyT] {
            private type m[+a] = _LazyT[a]
            override val selfMonad = _asMonad
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _LazyT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait _LazyT_ extends _LazyT_4 { this: _LazyT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _LazyT] = new MonadState[s, _LazyT] with MonadProxy[_LazyT] {
            private type m[+a] = _LazyT[a]
            override val selfMonad = _asMonad
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }
}
