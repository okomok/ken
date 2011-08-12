

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _MaybeTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerForComp[a](x: n[a]): inner.ForComp[a] = inner.forComp(x)
    private[this] implicit def innerInfix_>>=[a](x: n[a]): inner.Infix_>>=[a] = inner.>>=(x)

    sealed abstract class _MaybeT[+a] extends Strong[n[Maybe[a]]]

    object _MaybeT extends Kind.MonadTrans with Instance {
        override type apply[+a] = _MaybeT[a]
        override type inner[+a] = n[a]
        override type weak[+a] = n[Maybe[a]]

        def apply[a](rep: n[Maybe[a]]): _MaybeT[a] = new _MaybeT[a] {
            override def get: n[Maybe[a]] = rep
        }

        implicit def from[a](n: Strong[n[Maybe[a]]]): _MaybeT[a] = _MaybeT { n.run }

        def run[a](n: _MaybeT[a]): n[Maybe[a]] = n.run

        def map[m[+_], a, b](f: n[Maybe[a]] => m[Maybe[b]])(n: _MaybeT[a]): Strong[m[Maybe[b]]] = Strong { f(run(n)) }
    }

    private[ken] trait Instance0 { this: _MaybeT.type =>
        implicit val weak: Imply1[_MaybeT, ({type d[+a] = n[Maybe[a]]})#d] =
            new Imply1[_MaybeT, ({type d[+a] = n[Maybe[a]]})#d]
        {
            private[this] type p[+a] = _MaybeT[a]
            private[this] type d[+a] = n[Maybe[a]]
            override def imply1[a](p: p[a]): d[a] = run(p)
            override def unimply1[a](d: => d[a]): p[a] = _MaybeT(d)
        }

        implicit val _monad: MonadPlus[_MaybeT] = new MonadPlus[_MaybeT] {
            // Monad
            private[this] type m[+a] = _MaybeT[a]
            override def `return`[a](a: => a): m[a] = _MaybeT { inner.`return`(Just(a).up) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _MaybeT {
                run(m) >>= {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(v) => run(k(v))
                }
            }
            // MonadPlus
            override def mzero: m[Nothing] = _MaybeT { inner.`return`(Nothing) }
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = _MaybeT {
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
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_MaybeT] =
            new MonadIO[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override def self = _monad
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _MaybeT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_MaybeT] =
            new MonadCont[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _MaybeT {
                i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                    run( f( a => _MaybeT { c(Just(a)) } ) )
                }
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _MaybeT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _MaybeT] =
            new MonadError[e, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _MaybeT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _MaybeT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _MaybeT] =
            new MonadReader[r, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _monad
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _MaybeT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance5 extends Instance4 { this: _MaybeT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _MaybeT] =
            new MonadState[s, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = _monad
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }

    private[ken] trait Instance extends Instance5 { this: _MaybeT.type =>
    }
}
