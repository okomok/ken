

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _ListTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _ListT[+a] extends Strong[n[List[a]]]

    object _ListT extends Kind.MonadTrans with Instance {
        override type apply[+a] = _ListT[a]
        override type inner[+a] = n[a]
        override type weak[+a] = n[List[a]]

        def apply[a](rep: n[List[a]]): _ListT[a] = new _ListT[a] {
            override def get: n[List[a]] = rep
        }

        implicit def from[a](n: Strong[n[List[a]]]): _ListT[a] = _ListT { n.run }

        def run[a](n: _ListT[a]): n[List[a]] = n.run

        def map[m[+_], a, b](f: n[List[a]] => m[List[b]])(n: _ListT[a]): Strong[m[List[b]]] = Strong { f(run(n)) }
    }

    private[ken] trait Instance0 { this: _ListT.type =>
        implicit val weak: Imply1[_ListT, ({type d[+a] = n[List[a]]})#d] =
            new Imply1[_ListT, ({type d[+a] = n[List[a]]})#d]
        {
            private[this] type p[+a] = _ListT[a]
            private[this] type d[+a] = n[List[a]]
            override def imply1[a](p: p[a]): d[a] = run(p)
            override def unimply1[a](d: => d[a]): p[a] = _ListT(d)
        }

        implicit val _monad: MonadPlus[_ListT] = new MonadPlus[_ListT] {
            // Functor
            private[this] type f[+a] = _ListT[a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ListT {
                for { a <- run(m) } yield List.map(f)(a)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _ListT { inner.`return`(List(a)) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ListT {
                for { a <- run(m); b <- inner.mapM(run[b]_ compose k)(a) } yield List.concat(b)
            }
            // MonadPlus
            override def mzero: m[Nothing] = _ListT { inner.`return`(Nil) }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = _ListT {
                for { a <- run(m); b <- run(n) } yield a ::: b
            }
        }

        implicit val _asMonadTrans: MonadTrans[n, _ListT] = new MonadTrans[n, _ListT] {
            private[this] type m[+a] = _ListT[a]
            override def lift[a](n: n[a]): m[a] = _ListT {
                for { a <- n } yield List(a)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _ListT.type =>
        implicit def _asMonadIO(implicit i: MonadIO[n]): MonadIO[_ListT] =
            new MonadIO[_ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override def self = _monad
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _ListT.type =>
        implicit def _asMonadCont(implicit i: MonadCont[n]): MonadCont[_ListT] =
            new MonadCont[_ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = _monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ListT {
                i.callCC { (c: List[a] => n[List[b]]) =>
                    run( f( a => _ListT { c(List(a)) } ) )
                }
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _ListT.type =>
        implicit def _asMonadError[e](implicit i: MonadError[e, n]): MonadError[e, _ListT] =
            new MonadError[e, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = _monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ListT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _ListT.type =>
        implicit def _asMonadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _ListT] =
            new MonadReader[r, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = _monad
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _ListT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance5 extends Instance4 { this: _ListT.type =>
        implicit def _asMonadState[s](implicit i: MonadState[s, n]): MonadState[s, _ListT] =
            new MonadState[s, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = _monad
            override def get: m[s] = _asMonadTrans.lift(i.get)
            override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
        }
    }

    private[ken] trait Instance extends Instance5 { this: _ListT.type =>
    }
}
