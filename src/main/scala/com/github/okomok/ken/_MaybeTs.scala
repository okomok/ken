

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// MaybeT + reason == ErrorT


private[ken] final class _MaybeTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _MaybeT[+a](override val get: n[Maybe[a]]) extends Strong[n[Maybe[a]]]

    object _MaybeT extends _MaybeT_ with MonadPlus[_MaybeT] with Kind.AbstractMonadTrans {
        override type oldtype1[+a] = n[Maybe[a]]
        override type innerMonad[+a] = n[a]

        implicit def dependent[a](n: Strong[n[Maybe[a]]]): _MaybeT[a] = _MaybeT { n.run }

        def run[a](n: _MaybeT[a]): n[Maybe[a]] = n.run

        def map[m[+_], a, b](f: n[Maybe[a]] => m[Maybe[b]])(n: _MaybeT[a]): Strong[m[Maybe[b]]] = Strong { f(run(n)) }

        // Overrides
        //
        // Monad
        private type m[+a] = _MaybeT[a]
        override def `return`[a](a: Lazy[a]): m[a] = _MaybeT { inner.`return`(Just(a.!).up) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _MaybeT {
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(r) => run(k(r))
                }
            } yield *
        }
        // MonadPlus
        override def mzero: m[Nothing] = _MaybeT { inner.`return`(Nothing) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _MaybeT {
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => run(n)
                    case Just(r) => inner.`return`(Just(r))
                }
            } yield *
        }
    }

    private[ken] trait _MaybeT_0 { this: _MaybeT.type =>
        implicit val _asNewtype1: Newtype1[_MaybeT, ({type ot[+a] = n[Maybe[a]]})#ot] = new Newtype1[_MaybeT, ({type ot[+a] = n[Maybe[a]]})#ot] {
            private type nt[+a] = _MaybeT[a]
            private type ot[+a] = n[Maybe[a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _MaybeT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit val _asMonadPlus: MonadPlus[_MaybeT] = this

        implicit val _asMonadTrans: MonadTrans[n, _MaybeT] = new MonadTrans[n, _MaybeT] {
            private type m[+a] = _MaybeT[a]
            override def lift[a](n: n[a]): m[a] = _MaybeT {
                for { a <- n } yield Just(a)
            }
        }
    }

    private[ken] trait _MaybeT_1 extends _MaybeT_0 { this: _MaybeT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_MaybeT] = new MonadIO[_MaybeT] with MonadProxy[_MaybeT] {
            private type m[+a] = _MaybeT[a]
            override def selfMonad = _asMonadPlus
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait _MaybeT_2 extends _MaybeT_1 { this: _MaybeT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_MaybeT] = new MonadCont[_MaybeT] with MonadProxy[_MaybeT] {
            private type m[+a] = _MaybeT[a]
            override val selfMonad = _asMonadPlus
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _MaybeT {
                i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                    run( f( a => _MaybeT { c(Just(a)) } ) )
                }
            }
        }
    }

    private[ken] trait _MaybeT_3 extends _MaybeT_2 { this: _MaybeT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _MaybeT] = new MonadError[e, _MaybeT] with MonadProxy[_MaybeT] {
            private type m[+a] = _MaybeT[a]
            override val selfMonad = _asMonadPlus
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _MaybeT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait _MaybeT_4 extends _MaybeT_3 { this: _MaybeT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _MaybeT] = new MonadReader[r, _MaybeT] with MonadProxy[_MaybeT] {
            private type m[+a] = _MaybeT[a]
            override val selfMonad = _asMonadPlus
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _MaybeT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait _MaybeT_ extends _MaybeT_4 { this: _MaybeT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _MaybeT] = new MonadState[s, _MaybeT] with MonadProxy[_MaybeT] {
            private type m[+a] = _MaybeT[a]
            override val selfMonad = _asMonadPlus
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }
}
