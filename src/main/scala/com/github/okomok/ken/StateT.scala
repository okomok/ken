

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _StateTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _StateT[s, +a] extends Strong[s => n[(a, s)]]

    object _StateT extends Kind.Function1nv with Instance {
        sealed trait apply[s] extends Kind.MonadTrans {
            override type apply[+a] = _StateT[s, a]
            override type inner[+a] = n[a]
            override type weak[+a] = s => n[(a, s)]
        }

        def apply[s, a](rep: s => n[(a, s)]): _StateT[s, a] = new _StateT[s, a] {
            override def get: s => n[(a, s)] = rep
        }

        implicit def from[s, a](n: Strong[s => n[(a, s)]]): _StateT[s, a] = _StateT { n.run }

        def run[s, a](n: _StateT[s, a]): s => n[(a, s)] = n.run

        def eval[s, a](n: _StateT[s, a]): s => n[a] = s => for { (a, _) <- run(n)(s) } yield a

        def exec[s, a](n: _StateT[s, a]): s => n[s] = s => for { (_, s) <- run(n)(s) } yield s

        def map[s, m[+_], a, b](f: n[(a, s)] => m[(b, s)])(n: _StateT[s, a]): Strong[s => m[(b, s)]] = Strong { f compose run(n) }

        def `with`[s, a](f: s => s)(n: _StateT[s, a]): _StateT[s, a] = _StateT { run(n) compose f }
    }

    private[ken] trait Instance0 { this: _StateT.type =>
        implicit def weak[s]: Imply1[({type p[+a] = _StateT[s, a]})#p, ({type d[+a] = s => n[(a, s)]})#d] =
            new Imply1[({type p[+a] = _StateT[s, a]})#p, ({type d[+a] = s => n[(a, s)]})#d]
        {
            private[this] type p[+a] = _StateT[s, a]
            private[this] type d[+a] = s => n[(a, s)]
            override def imply[a](p: p[a]): d[a] = run(p)
            override def unimply[a](d: => d[a]): p[a] = _StateT(d)
        }

        implicit def _monad[s]: MonadState[s, ({type m[+a] = _StateT[s, a]})#m] = new MonadState[s, ({type m[+a] = _StateT[s, a]})#m]
        {
            // Functor
            private[this] type f[+a] = _StateT[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _StateT { s =>
                for { (x, s_) <- run(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _StateT { s => inner.`return`(a, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _StateT { s =>
                for { (a, s_) <- run(m)(s); * <- run(k(a))(s_) } yield *
            }
            // MonadState
            override def get: m[s] = _StateT { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = _StateT { _ => inner.`return`((), s) }
        }

        implicit def _asMonadTrans[s]: MonadTrans[n, ({type m[+a] = _StateT[s, a]})#m] = new MonadTrans[n, ({type m[+a] = _StateT[s, a]})#m] {
            private[this] type m[+a] = _StateT[s, a]
            override def lift[a](n: n[a]): m[a] = _StateT { s => for { a <- n } yield (a, s) }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _StateT.type =>
        implicit def _asMonadPlus[s](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = _StateT[s, a]})#m] =
            new MonadPlus[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def mzero: m[Nothing] = _StateT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = _StateT { s => i.mplus(run(m)(s))(run(n)(s)) }
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _StateT.type =>
        implicit def _asMonadFix[s](implicit i: MonadFix[n]): MonadFix[({type m[+a] = _StateT[s, a]})#m] =
            new MonadFix[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def mfix[a](f: (=> a) => m[a]): m[a] = _StateT { s =>
                def k(aI_ : => (a, s)) = run(f(aI_._1))(s)
                i.mfix(k)
                // scalac sucks.
                // i.mfix { (aI_ : (=> (a, s))) => run(f(aI_._1))(s) }
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _StateT.type =>
        implicit def _asMonadIO[s](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _StateT[s, a]})#m] =
            new MonadIO[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _StateT.type =>
        implicit def _asMonadCont[s](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _StateT[s, a]})#m] =
            new MonadCont[({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _StateT { s =>
                i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                    run( f( a => _StateT { s_ => c((a, s_)) } ) )(s)
                }
            }
        }
    }

    private[ken] trait Instance5 extends Instance4 { this: _StateT.type =>
        implicit def _asMonadError[s, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = _StateT[s, a]})#m] =
            new MonadError[e, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _StateT { s =>
                i.catchError(run(m)(s)) { e => run(h(e))(s) }
            }
        }
    }

    private[ken] trait Instance6 extends Instance5 { this: _StateT.type =>
        implicit def _asMonadReader[s, r](implicit i: MonadReader[r, n]): MonadReader[r, ({type m[+a] = _StateT[s, a]})#m] =
            new MonadReader[r, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _StateT { s => i.local(f)(run(m)(s)) }
        }
    }

    private[ken] trait Instance7 extends Instance6 { this: _StateT.type =>
        implicit def _asMonadWriter[s, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = _StateT[s, a]})#m] =
            new MonadWriter[w, ({type m[+a] = _StateT[s, a]})#m] with MonadProxy[({type m[+a] = _StateT[s, a]})#m]
        {
            private[this] type m[+a] = _StateT[s, a]
            override val self = _monad[s]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = _StateT { s =>
                for { ((a, s_), w) <- i.listen(run(m)(s)) } yield ((a, w), s_)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = _StateT { s =>
                i.pass {
                    for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
                }
            }
        }
    }

    private[ken] trait Instance extends Instance7 { this: _StateT.type =>
    }
}
