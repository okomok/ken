

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _ListTs[n[+_]](val inner: Monad[n]) {

    type _ListT[+a] = n[List[a]]

    trait LowPriorityImplicits { this: _ListT.type =>
        implicit val monad: MonadPlus[_ListT] with inner.Trans[_ListT] = new MonadPlus[_ListT] with inner.Trans[_ListT] {
            // Functor
            private[this] type f[+a] = _ListT[a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = {
                import inner.`for`
                for { a <- m } yield List.map(f)(a)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = inner.`return`(List(a))
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = {
                import inner.`for`
                for { a <- m; b <- inner.mapM(k)(a) } yield List.concat(b)
            }
            // MonadPlus
            override def mzero: m[Nothing] = inner.`return`(Nil)
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = {
                import inner.`for`
                for { a <- m; b <- n } yield a ::: b
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = {
                import inner.`for`
                for { a <- n } yield List(a)
            }
        }
    }

    object _ListT extends LowPriorityImplicits {
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
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = {
                i.callCC { (c: List[a] => m[b]) => f(a => c(List(a))) }
            }
        }

        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _ListT] =
            new MonadError[e, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = {
                i.catchError(m) { e => h(e) }
            }
        }

        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _ListT] =
            new MonadReader[r, _ListT] with MonadProxy[_ListT]
        {
            private[this] type m[+a] = _ListT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = i.local(f)(m)
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
