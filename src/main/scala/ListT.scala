

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _ListTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _ListT[+a] extends Run[n[List[a]]]

    object _ListT extends Instances {
        def apply[a](rep: n[List[a]]): _ListT[a] = new _ListT[a] {
            override def run: n[List[a]] = rep
        }

        implicit def from[a](n: Run[n[List[a]]]): _ListT[a] = _ListT { n.run }

        def run[a](n: _ListT[a]): n[List[a]] = n.run

        def map[m[+_], a, b](f: n[List[a]] => m[List[b]])(n: _ListT[a]): Run[m[List[b]]] = Run { f(run(n)) }
    }

    trait LowPriorityInstances { this: _ListT.type =>
        implicit val monad: MonadPlus[_ListT] with inner.Trans[_ListT] = new MonadPlus[_ListT] with inner.Trans[_ListT] {
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
            // Trans
            override def lift[a](n: n[a]): m[a] = _ListT {
                for { a <- n } yield List(a)
            }
        }
    }

    trait Instances extends LowPriorityInstances { this: _ListT.type =>
        implicit def monadIO(implicit i: MonadIO[n]): MonadIO[_ListT] =
            new MonadIO[_ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override def self = monad
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont(implicit i: MonadCont[n]): MonadCont[_ListT] =
            new MonadCont[_ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ListT {
                i.callCC { (c: List[a] => n[List[b]]) =>
                    run( f( a => _ListT { c(List(a)) } ) )
                }
            }
        }

        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _ListT] =
            new MonadError[e, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ListT {
                i.catchError(run(m)) { e => run(h(e)) }
            }
        }

        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _ListT] =
            new MonadReader[r, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _ListT { i.local(f)(run(m)) }
        }

        implicit def monadState[s](implicit i: MonadState[s, n]): MonadState[s, _ListT] =
            new MonadState[s, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }
}
