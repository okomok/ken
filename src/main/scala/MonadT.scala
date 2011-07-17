

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Monad Transformers
 */
final class MonadT[n[+_]](implicit val inner: Monad[n]) {

// Workaround Monad.method.
    private[this] implicit def innermethod[a](x: n[a]): MonadMethod[n, a] = inner.method(x)

// Trans
    trait Trans[m[+_]] {
        def lift[a](n: n[a]): m[a]
    }

    object Trans {
        def apply[m[+_]](implicit i: Trans[m]): Trans[m] = i
    }

// MaybeT
    sealed abstract class MaybeT[+a] {
        def run: n[Maybe[a]]
    }

    object MaybeT extends MonadPlus[MaybeT] with Trans[MaybeT] {
        def apply[a](_run: => n[Maybe[a]]): MaybeT[a] = new MaybeT[a] {
            override def run: n[Maybe[a]] = _run
        }

        def run[a](n: MaybeT[a]): n[Maybe[a]] = n.run

        // Monad
        private[this] type m[+a] = MaybeT[a]
        override def `return`[a](x: a): m[a] = MaybeT { inner.`return`(Just(x).up) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = MaybeT {
            run(x) >>= {
                case Nothing => inner.`return`(Nothing.of[b])
                case Just(v) => run(y(v))
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { inner.`return`(Nothing) }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = MaybeT {
            run(x) >>= {
                case Nothing => run(y)
                case Just(_) => run(x)
            }
        }
        // Trans
        override def lift[a](n: n[a]): m[a] = MaybeT { inner.liftM(Maybe.just[a])(n) }
    }

// StateT
    sealed abstract class StateT[s, +a] extends MonadMethod[({type m[+a] = StateT[s, a]})#m, a] {
        def run(s: s): n[(a, s)]
        override val klass = StateT.monad[s]
        override def callee = this
    }

    object StateT {
        def apply[s, a](_run: s => n[(a, s)]): StateT[s, a] = new StateT[s, a] {
            override def run(s: s): n[(a, s)] = _run(s)
        }

        def run[s, a](n: StateT[s, a]): s => n[(a, s)] = n.run

        def eval[s, a](n: StateT[s, a])(s: s): n[a] = for { (a, _) <- run(n)(s) } yield a

        def exec[s, a](n: StateT[s, a])(s: s): n[s] = for { (_, s) <- run(n)(s) } yield s

        def map[s, a, b](f: n[(a, s)] => n[(b, s)])(n: StateT[s, a]): StateT[s, b] = StateT { f compose run(n) }

        def `with`[s, a](f: s => s)(n: StateT[s, a]): StateT[s, a] = StateT { run(n) compose f }

        implicit def monad[s]: MonadState[s, ({type m[+a] = StateT[s, a]})#m] =
            new MonadState[s, ({type m[+a] = StateT[s, a]})#m]
        {
            // Functor
            private[this] type f[+a] = StateT[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = StateT { s =>
                for { (x, s_) <- run(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = StateT { s => inner.`return`(a, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
                for { (a, s_) <- run(m)(s); * <- run(k(a))(s_) } yield *
            }
            // MonadState
            override def get: m[s] = StateT { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = StateT { _ => inner.`return`((), s) }
        }

        implicit def monadPlus[s](implicit i: MonadPlus[n]): MonadPlus[({type m[+a] = StateT[s, a]})#m] =
            new MonadPlus[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def mzero: m[Nothing] = StateT { _ => i.mzero }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = StateT { s => i.mplus(run(m)(s))(run(n)(s)) }
        }

        implicit def monadFix[s](implicit i: MonadFix[n]): MonadFix[({type m[+a] = StateT[s, a]})#m] =
            new MonadFix[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def mfix[a](f: (=> a) => m[a]): m[a] = StateT { s =>
                def k(aI_ : => (a, s)) = run(f(aI_._1))(s)
                i.mfix(k)
                // scalac sucks.
                // i.mfix { (aI_ : => (a, s)) => run(f(aI_._1))(s) }
            }
        }

        implicit def monadTrans[s]: Trans[({type m[+a] = StateT[s, a]})#m] = new Trans[({type m[+a] = StateT[s, a]})#m] {
            private[this] type m[+a] = StateT[s, a]
            override def lift[a](n: n[a]): m[a] = StateT { s => for { a <- n } yield (a, s) }
        }

        implicit def monadIO[s](implicit i: MonadIO[n]): MonadIO[({type m[+a] = StateT[s, a]})#m] =
            new MonadIO[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def liftIO[a](io: IO[a]): m[a] = monadTrans[s].lift(i.liftIO(io))
        }
/*
        implicit def monadCont[s](implicit i: MonadCont[n]): MonadCont[({type m[+a] = StateT[s, a]})#m] =
            new MonadCont[({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
                i.callCC { (c: (a, s) => n[(a, s)]) =>
                    run(f(a => StateT { s_ => c(a, s_) }))(s)
                }
            }
        }
*/
        implicit def monadError[s, e](implicit i: MonadError[e, n]): MonadError[e, ({type m[+a] = StateT[s, a]})#m] =
            new MonadError[e, ({type m[+a] = StateT[s, a]})#m] with MonadProxy[({type m[+a] = StateT[s, a]})#m]
        {
            private[this] type m[+a] = StateT[s, a]
            override val self = monad[s]
            override def throwError[a](e: e): m[a] = monadTrans[s].lift(i.throwError(e))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = StateT { s =>
                i.catchError(run(m)(s)) { e =>
                    run(h(e))(s)
                }
            }
        }
    }

// ReaderT
    sealed abstract class ReaderT[r, +a] extends MonadMethod[({type m[+a] = ReaderT[r, a]})#m, a] {
        def run(r: r): n[a]
        override val klass = ReaderT.monad[r]
        override def callee = this
    }

    object ReaderT {
        def apply[r, a](_run: r => n[a]): ReaderT[r, a] = new ReaderT[r, a] {
            override def run(r: r): n[a] = _run(r)
        }

        def run[r, a](n: ReaderT[r, a]): r => n[a] = n.run

        def map[r, a, b](f: n[a] => n[b])(n: ReaderT[r, a]): ReaderT[r, b] = ReaderT { f compose run(n) }

        def `with`[r, r_, a](f: r_ => r)(n: ReaderT[r, a]): ReaderT[r_, a] = ReaderT { run(n) compose f }

        implicit def monad[r]: Monad[({type m[+a] = ReaderT[r, a]})#m] with Trans[({type m[+a] = ReaderT[r, a]})#m] =
            new Monad[({type m[+a] = ReaderT[r, a]})#m] with Trans[({type m[+a] = ReaderT[r, a]})#m]
        {
            // Functor
            private[this] type f[+a] = ReaderT[r, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = ReaderT { r =>
                for { a <- run(m)(r) } yield f(a)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = ReaderT { r => inner.`return`(a) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ReaderT { r =>
                for { a <- run(m)(r); * <- run(k(a))(r) } yield *
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = ReaderT { _ => n }
        }
    }

// WriterT
    final class WriterT[w, +a](_run: () => n[(a, w)])(implicit i: Monoid[w]) extends MonadMethod[({type m[+a] = WriterT[w, a]})#m, a] {
        def run: n[(a, w)] = _run()
        override val klass = WriterT.monad[w]
        override def callee = this
    }

    object WriterT {
        def apply[w, a](_run: => n[(a, w)])(implicit i: Monoid[w]): WriterT[w, a] = new WriterT[w, a](() => _run)

        def run[w, a](n: WriterT[w, a]): n[(a, w)] = n.run

        def exec[w, a](n: WriterT[w, a]): n[w] = for { (_, w) <- run(n) } yield w

        def map[w, w_, a, b](f: n[(a, w)] => n[(b, w_)])(n: WriterT[w, a])(implicit i: Monoid[w_]): WriterT[w_, b] = WriterT { f(run(n)) }

        implicit def monad[w](implicit i: Monoid[w]): Monad[({type m[+a] = WriterT[w, a]})#m] with Trans[({type m[+a] = WriterT[w, a]})#m] =
            new Monad[({type m[+a] = WriterT[w, a]})#m] with Trans[({type m[+a] = WriterT[w, a]})#m]
        {
            // Functor
            private[this] type f[+a] = WriterT[w, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = WriterT {
                for { (a, w) <- run(m) } yield (f(a), w)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = WriterT { inner.`return`(a, i.mempty) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = WriterT {
                for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, i.mappend(w)(w_))
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = WriterT {
                for { a <- n } yield (a, i.mempty)
            }
        }
    }

}
