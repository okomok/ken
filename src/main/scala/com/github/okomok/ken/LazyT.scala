

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _LazyTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _LazyT[+a] extends Identity[n[Lazy[a]]]

    object _LazyT extends Instances {
        def apply[a](rep: n[Lazy[a]]): _LazyT[a] = new _LazyT[a] {
            override def run: n[Lazy[a]] = rep
        }

        implicit def from[a](n: Identity[n[Lazy[a]]]): _LazyT[a] = _LazyT { n.run }

        def run[a](n: _LazyT[a]): n[Lazy[a]] = n.run

        def map[m[+_], a, b](f: n[Lazy[a]] => m[Lazy[b]])(n: _LazyT[a]): Identity[m[Lazy[b]]] = Identity { f(run(n)) }
    }

    private[ken] trait Instance0 { this: _LazyT.type =>
        implicit val weak: Weak[_LazyT, ({type d[+a] = n[Lazy[a]]})#d] =
            new Weak[_LazyT, ({type d[+a] = n[Lazy[a]]})#d]
        {
            private[this] type p[+a] = _LazyT[a]
            private[this] type d[+a] = n[Lazy[a]]
            override def wrap[a](d: => d[a]): p[a] = _LazyT(d)
            override def unwrap[a](p: p[a]): d[a] = run(p)
        }

        implicit val monad: Monad[_LazyT] with inner.Trans[_LazyT] = new Monad[_LazyT] with inner.Trans[_LazyT] {
            // Functor
            private[this] type f[+a] = _LazyT[a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _LazyT {
                for { a <- run(m) } yield Lazy(f(a.!))
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _LazyT { inner.`return`(Lazy(a)) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _LazyT {
                for { a <- run(m); * <- run(k(a.!)) } yield *
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = _LazyT {
                for { a <- n } yield Lazy(a)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _LazyT.type =>
        implicit def monadIO(implicit i: MonadIO[n]): MonadIO[_LazyT] =
            new MonadIO[_LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override def self = monad
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _LazyT.type =>
        implicit def monadCont(implicit i: MonadCont[n]): MonadCont[_LazyT] =
            new MonadCont[_LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _LazyT {
                i.callCC { (c: Lazy[a] => n[Lazy[b]]) =>
                    run( f( a => _LazyT { c(Lazy(a)) } ) )
                }
            }
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _LazyT.type =>
        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _LazyT] =
            new MonadError[e, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _LazyT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _LazyT.type =>
        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _LazyT] =
            new MonadReader[r, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _LazyT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance5 extends Instance4 { this: _LazyT.type =>
        implicit def monadState[s](implicit i: MonadState[s, n]): MonadState[s, _LazyT] =
            new MonadState[s, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }

    private[ken] trait Instances extends Instance5 { this: _LazyT.type =>
    }
}