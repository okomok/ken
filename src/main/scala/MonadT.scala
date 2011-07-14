

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Monad Transformers
 */
trait MonadT[n[+_]] {
    implicit val inner: Monad[n]

// Trans
    trait Trans[m[+_]] extends Klass {
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
        override implicit def instance = this

        def apply[a](r: n[Maybe[a]]): MaybeT[a] = new MaybeT[a] {
            override def run: n[Maybe[a]] = r
        }

        def run[a](n: MaybeT[a]): n[Maybe[a]] = n.run

        // Monad
        private[this] type m[+a] = MaybeT[a]
        override def `return`[a](x: a): m[a] = MaybeT { inner.`return`(Just(x).up) }
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = MaybeT {
            import inner.method
            run(x) >>= {
                case Nothing => inner.`return`(Nothing.of[b])
                case Just(v) => run(y(v))
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { inner.`return`(Nothing) }
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = MaybeT {
            import inner.method
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
        import inner.method

        def apply[s, a](r: s => n[(a, s)]): StateT[s, a] = new StateT[s, a] {
            override def run(s: s): n[(a, s)] = r(s)
        }

        def run[s, a](n: StateT[s, a]): s => n[(a, s)] = n.run

        def eval[s, a](n: StateT[s, a])(s: s): n[a] = for { (a, _) <- run(n)(s) } yield a

        def exec[s, a](n: StateT[s, a])(s: s): n[s] = for { (_, s) <- run(n)(s) } yield s

        def map[s, a, b](f: n[(a, s)] => n[(b, s)])(n: StateT[s, a]): StateT[s, b] = StateT { f compose run(n) }

        def `with`[s, a](f: s => s)(n: StateT[s, a]): StateT[s, a] = StateT { run(n) compose f }

        implicit def monad[s]: Monad[({type m[+a] = StateT[s, a]})#m] with MonadState[s, ({type m[+a] = StateT[s, a]})#m] =
            new Monad[({type m[+a] = StateT[s, a]})#m] with MonadState[s, ({type m[+a] = StateT[s, a]})#m]
        {
            // Functor
            private[this] type f[+a] = StateT[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = StateT { s =>
                import inner.method // works around Monad.method.
                for { (x, s_) <- run(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = StateT { s => inner.`return`(a, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
                import inner.method
                for { (a, s_) <- run(m)(s); r <- run(k(a))(s_) } yield r
            }
            // MonadState
            override def get: m[s] = StateT { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = StateT { _ => inner.`return`((), s) }
        }

        implicit def trans[s]: Trans[({type m[+a] = StateT[s, a]})#m] = new Trans[({type m[+a] = StateT[s, a]})#m] {
            private[this] type m[+a] = StateT[s, a]
            override def lift[a](n: n[a]): m[a] = StateT { s => for { a <- n } yield (a, s) }
        }
    }
}
