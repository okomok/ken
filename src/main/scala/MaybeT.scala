

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _MaybeTs[n[+_]](val inner: Monad[n]) {

    type _MaybeT[+a] = n[Maybe[a]]

    trait LowPriorityImplicits { this: _MaybeT.type =>
        implicit val monad: MonadPlus[_MaybeT] with inner.Trans[_MaybeT] = new MonadPlus[_MaybeT] with inner.Trans[_MaybeT] {
            // Monad
            private[this] type m[+a] = _MaybeT[a]
            override def `return`[a](a: => a): m[a] = inner.`return`(Just(a).up)
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = {
                import inner.>>=
                m >>= {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(v) => k(v)
                }
            }
            // MonadPlus
            override def mzero: m[Nothing] = inner.`return`(Nothing)
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = {
                import inner.>>=
                x >>= {
                    case Nothing => y
                    case Just(_) => x
                }
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = inner.liftM(Maybe.just[a])(n)
        }
    }

    object _MaybeT extends LowPriorityImplicits {
        implicit def monadIO(implicit i: MonadIO[n]): MonadIO[_MaybeT] =
            new MonadIO[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override def self = monad
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont(implicit i: MonadCont[n]): MonadCont[_MaybeT] =
            new MonadCont[_MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = {
                i.callCC { (c: Maybe[a] => m[b]) => f(a => c(Just(a))) }
            }
        }

        implicit def monadError[e](implicit i: MonadError[e, n]): MonadError[e, _MaybeT] =
            new MonadError[e, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def errorClass: ErrorClass[e] = i.errorClass
            override def throwError[a](e: e): m[a] = self.lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = {
                i.catchError(m) { e => h(e) }
            }
        }

        implicit def monadReader[r](implicit i: MonadReader[r, n]): MonadReader[r, _MaybeT] =
            new MonadReader[r, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = i.local(f)(m)
        }

        implicit def monadState[s](implicit i: MonadState[s, n]): MonadState[s, _MaybeT] =
            new MonadState[s, _MaybeT] with MonadProxy[_MaybeT]
        {
            private[this] type m[+a] = _MaybeT[a]
            override val self = monad
            override def get: m[s] = self.lift(i.get)
            override def put(s: s): m[Unit] = self.lift(i.put(s))
        }
    }
}
