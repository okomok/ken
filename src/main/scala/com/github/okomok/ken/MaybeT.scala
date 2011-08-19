

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _MaybeTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerForComp[a](x: n[a]): inner.ForComp[a] = inner.forComp(x)
    private[this] implicit def innerOp_>>=[a](x: n[a]): inner.Op_>>=[a] = inner.>>=(x)

    final case class _MaybeT[+a](override val get: n[Maybe[a]]) extends NewtypeOf[n[Maybe[a]]]

    object _MaybeT extends Instance with Kind.AbstractMonadTrans {
        override type apply1[+a] = _MaybeT[a]
        override type oldtype1[+a] = n[Maybe[a]]
        override type innerMonad[+a] = n[a]

        def run[a](n: _MaybeT[a]): n[Maybe[a]] = n.run

        def map[m[+_], a, b](f: n[Maybe[a]] => m[Maybe[b]])(n: _MaybeT[a]): NewtypeOf[m[Maybe[b]]] = NewtypeOf { f(run(n)) }

        implicit def dependent[a](n: NewtypeOf[n[Maybe[a]]]): _MaybeT[a] = _MaybeT { n.run }
    }

    private[ken] trait Instance0 { this: _MaybeT.type =>
        implicit val _asNewtype1: Newtype1[_MaybeT, ({type ot[+a] = n[Maybe[a]]})#ot] = new Newtype1[_MaybeT, ({type ot[+a] = n[Maybe[a]]})#ot] {
            private[this] type nt[+a] = _MaybeT[a]
            private[this] type ot[+a] = n[Maybe[a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _MaybeT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit val _asMonadPlus: MonadPlus[_MaybeT] = new MonadPlus[_MaybeT] {
            // Monad
            private[this] type m[+a] = _MaybeT[a]
            override def `return`[a](a: Lazy[a]): m[a] = _MaybeT { inner.`return`(Just(a.!).up) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _MaybeT {
                run(m) >>= {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(v) => run(k(v))
                }
            }
            // MonadPlus
            override def mzero: m[Nothing] = _MaybeT { inner.`return`(Nothing) }
            override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = _MaybeT {
                val runx = run(x)
                runx >>= {
                    case Nothing => run(y)
                    case Just(_) => runx
                }
            }
        }

        implicit val _asMonadTrans: MonadTrans[n, _MaybeT] = new MonadTrans[n, _MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override def lift[a](n: n[a]): m[a] = _MaybeT {
                for { a <- n } yield Just(a)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _MaybeT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_MaybeT] = new MonadIO[_MaybeT] with MonadProxy[_MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override def self = _asMonadPlus
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _MaybeT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_MaybeT] = new MonadCont[_MaybeT] with MonadProxy[_MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _asMonadPlus
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _MaybeT {
                i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                    run( f( a => _MaybeT { c(Just(a)) } ) )
                }
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _MaybeT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _MaybeT] = new MonadError[e, _MaybeT] with MonadProxy[_MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _asMonadPlus
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _MaybeT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _MaybeT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _MaybeT] = new MonadReader[r, _MaybeT] with MonadProxy[_MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _asMonadPlus
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _MaybeT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance extends Instance4 { this: _MaybeT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _MaybeT] = new MonadState[s, _MaybeT] with MonadProxy[_MaybeT] {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _asMonadPlus
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }
}
