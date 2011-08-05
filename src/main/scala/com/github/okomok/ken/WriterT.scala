

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _WriterTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _WriterT[w, +a] extends Strong[n[(a, w)]]

    object _WriterT extends Instance {
        type apply[w] = Kind.MonadTrans {
            type apply[+a] = _WriterT[w, a]
            type inner[+a] = n[a]
        }

        def apply[w, a](rep: n[(a, w)]): _WriterT[w, a] = new _WriterT[w, a] {
            override def get: n[(a, w)] = rep
        }

        implicit def from[w, a](n: Strong[n[(a, w)]]): _WriterT[w, a] = _WriterT(n.run)

        def run[w, a](n: _WriterT[w, a]): n[(a, w)] = n.run

        def exec[w, a](n: _WriterT[w, a]): n[w] = for { (_, w) <- run(n) } yield w

        def map[w, w_, m[+_], a, b](f: n[(a, w)] => m[(b, w_)])(n: _WriterT[w, a]): Strong[m[(b, w_)]] = Strong { f(run(n)) }
    }

    private[ken] trait Instance0 { outer: _WriterT.type =>
        implicit def weak[w]: Weak1[({type p[+a] = _WriterT[w, a]})#p, ({type d[+a] = n[(a, w)]})#d] =
            new Weak1[({type p[+a] = _WriterT[w, a]})#p, ({type d[+a] = n[(a, w)]})#d]
        {
            private[this] type p[+a] = _WriterT[w, a]
            private[this] type d[+a] = n[(a, w)]
            override def wrap[a](d: => d[a]): p[a] = _WriterT(d)
            override def unwrap[a](p: p[a]): d[a] = run(p)
        }

        implicit def monad[w](implicit i: Monoid[w]): MonadWriter[w, ({type m[+a] = _WriterT[w, a]})#m] with inner.Trans[({type m[+a] = _WriterT[w, a]})#m] =
            new MonadWriter[w, ({type m[+a] = _WriterT[w, a]})#m] with inner.Trans[({type m[+a] = _WriterT[w, a]})#m]
        {
            // Functor
            private[this] type f[+a] = _WriterT[w, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _WriterT {
                for { (a, w) <- run(m) } yield (f(a), w)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _WriterT { inner.`return`(a, i.mempty) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _WriterT {
                for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, i.mappend(w)(w_))
            }
            // MonadWriter
            override def monoid: Monoid[w] = i
            override def tell(w: w): m[Unit] = _WriterT { inner.`return`((), w) }
            override def listen[a](m: m[a]): m[(a, w)] = _WriterT {
                for { (a, w) <- run(m) } yield ((a, w), w)
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = _WriterT {
                for { ((a, f), w) <- run(m) } yield (a, f(w))
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = _WriterT {
                for { a <- n } yield (a, i.mempty)
            }
        }

        implicit def asMonadTrans[w](implicit i: Monoid[w]): MonadTrans[n, ({type m[+a] = _WriterT[w, a]})#m] = new MonadTrans[n, ({type m[+a] = _WriterT[w, a]})#m] {
            private[this] type m[+a] = _WriterT[w, a]
            override def lift[a](n: n[a]): m[a] = _WriterT {
                for { a <- n } yield (a, i.mempty)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { outer: _WriterT.type =>
        implicit def asMonadPlus[w](implicit i: MonadPlus[n], j: Monoid[w]): MonadPlus[({type m[+a] = _WriterT[w, a]})#m] =
            new MonadPlus[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def mzero: m[Nothing] = _WriterT { i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = _WriterT { i.mplus(run(m))(run(n)) }
        }
    }

    private[ken] trait Instance2 extends Instance1 { outer: _WriterT.type =>
        implicit def asMonadFix[w](implicit i: MonadFix[n], j: Monoid[w]): MonadFix[({type m[+a] = _WriterT[w, a]})#m] =
            new MonadFix[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def mfix[a](m: (=> a) => m[a]): m[a] = _WriterT {
                def k(aI_ : => (a, w)) = run(m(aI_._1))
                i.mfix(k)
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { outer: _WriterT.type =>
        implicit def asMonadIO[w](implicit i: MonadIO[n], j: Monoid[w]): MonadIO[({type m[+a] = _WriterT[w, a]})#m] =
            new MonadIO[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance4 extends Instance3 { outer: _WriterT.type =>
        implicit def asMonadCont[w](implicit i: MonadCont[n], j: Monoid[w]): MonadCont[({type m[+a] = _WriterT[w, a]})#m] =
            new MonadCont[({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _WriterT {
                i.callCC { (c: ((a, w)) => n[(b, w)]) =>
                    run( f(a => _WriterT { c((a, j.mempty)) }) )
                }
            }
        }
    }

    private[ken] trait Instance5 extends Instance4 { outer: _WriterT.type =>
        implicit def asMonadError[w, e](implicit i: MonadError[e, n], j: Monoid[w]): MonadError[e, ({type m[+a] = _WriterT[w, a]})#m] =
            new MonadError[e, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _WriterT {
                i.catchError(run(m)) { e =>
                    run(h(e))
                }
            }
        }
    }

    private[ken] trait Instance6 extends Instance5 { outer: _WriterT.type =>
        implicit def asMonadReader[w, r](implicit i: MonadReader[r, n], j: Monoid[w]): MonadReader[r, ({type m[+a] = _WriterT[w, a]})#m] =
            new MonadReader[r, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _WriterT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance7 extends Instance6 { outer: _WriterT.type =>
        implicit def asMonadState[w, s](implicit i: MonadState[s, n], j: Monoid[w]): MonadState[s, ({type m[+a] = _WriterT[w, a]})#m] =
            new MonadState[s, ({type m[+a] = _WriterT[w, a]})#m] with MonadProxy[({type m[+a] = _WriterT[w, a]})#m]
        {
            private[this] type m[+a] = _WriterT[w, a]
            override val self = outer.monad[w]
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }

    private[ken] trait Instance extends Instance7 { outer: _WriterT.type =>
    }
}
