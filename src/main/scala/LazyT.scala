

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _LazyTs[n[+_]](val inner: Monad[n]) {

    type _LazyT[+a] = n[Lazy[a]]

    trait LowPriorityImplicits { this: _LazyT.type =>
        implicit val monad: Monad[_LazyT] with inner.Trans[_LazyT] = new Monad[_LazyT] with inner.Trans[_LazyT] {
            // Functor
            private[this] type f[+a] = _LazyT[a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = {
                import inner.`for`
                for { a <- m } yield Lazy(f(a.!))
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = inner.`return`(Lazy(a))
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = {
                import inner.`for`
                for { a <- m; * <- k(a.!) } yield *
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = inner.liftM((x: a) => Lazy(x))(n)
        }
    }

    object _LazyT extends LowPriorityImplicits {
        implicit def monadIO(implicit i: MonadIO[n]): MonadIO[_LazyT] =
            new MonadIO[_LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override def self = monad
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont(implicit i: MonadCont[n]): MonadCont[_LazyT] =
            new MonadCont[_LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = {
                i.callCC { (c: Lazy[a] => m[b]) => f(a => c(Lazy(a))) }
            }
        }

        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _LazyT] =
            new MonadError[e, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = {
                i.catchError(m) { e => h(e) }
            }
        }

        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _LazyT] =
            new MonadReader[r, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = i.local(f)(m)
        }

        implicit def monadState[s](implicit i: MonadState[s, n]): MonadState[s, _LazyT] =
            new MonadState[s, _LazyT] with MonadProxy[_LazyT]
        {
            private[this] type m[+a] = _LazyT[a]
            override val self = monad
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }
}
