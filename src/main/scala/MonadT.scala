

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Monad Transformers
 */
trait MonadT[n[+_]] {
    implicit val inner: Monad[n]

    sealed abstract class State[s, +a] extends MonadMethod[({type m[+a] = State[s, a]})#m, a] {
        def run(s: s): n[(a, s)]
        override val klass = State.monad[s]
        override def callee = this
    }

    object State {
        import inner.method

        def apply[s, a](r: s => n[(a, s)]): State[s, a] = new State[s, a] {
            override def run(s: s): n[(a, s)] = r(s)
        }

        def runState[s, a](n: State[s, a]): s => n[(a, s)] = n.run

        def evalState[s, a](n: State[s, a])(s: s): n[a] = for { (a, _) <- runState(n)(s) } yield a

        def execState[s, a](n: State[s, a])(s: s): n[s] = for { (_, s) <- runState(n)(s) } yield s

        def mapState[s, a, b](f: n[(a, s)] => n[(b, s)])(n: State[s, a]): State[s, b] = State { f compose runState(n) }

        def withState[s, a](f: s => s)(n: State[s, a]): State[s, a] = State { runState(n) compose f }

        implicit def monad[s]: Monad[({type m[+a] = State[s, a]})#m] with MonadState[s, ({type m[+a] = State[s, a]})#m] =
            new Monad[({type m[+a] = State[s, a]})#m] with MonadState[s, ({type m[+a] = State[s, a]})#m]
        {
            // Functor
            private[this] type f[+a] = State[s, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = State { s =>
                import inner.method // works around Monad.method.
                for { (x, s_) <- runState(m)(s) } yield (f(x), s_)
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = State { s => inner.`return`(a, s) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = State { s =>
                import inner.method
                for { (a, s_) <- runState(m)(s); r <- runState(k(a))(s_) } yield r
            }
            // MonadState
            override def get: m[s] = State { s => inner.`return`(s, s) }
            override def put(s: s): m[Unit] = State { _ => inner.`return`((), s) }
        }
    }
}
