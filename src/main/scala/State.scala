

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class State[s, +a] extends MonadMethod[({type m[+a] = State[s, a]})#m, a] {
    def run(s: s): (a, s)
    override val klass = State.monad[s]
    override def callee = this
}


object State {
    def apply[s, a](r: s => (a, s)): State[s, a] = new State[s, a] {
        override def run(s: s): (a, s) = r(s)
    }

    def `lazy`[s, a](m: => State[s, a]): State[s, a] = new State[s, a] {
        private[this] lazy val _m: State[s, a] = m
        override def run(s: s): (a, s) = _m.run(s)
    }

    def runState[s, a](m: State[s, a]): s => (a, s) = m.run

    def evalState[s, a](m: State[s, a])(s: s): a = fst(runState(m)(s))

    def execState[s, a](m: State[s, a])(s: s): s = snd(runState(m)(s))

    def mapState[s, a, b](f: (a, s) => (b, s))(m: State[s, a]): State[s, b] = State { f.tupled compose runState(m) }

    def withState[s, a](f: s => s)(m: State[s, a]): State[s, a] = State { runState(m) compose f }

    implicit def monad[s]: MonadFix[({type m[+a] = State[s, a]})#m] with MonadState[s, ({type m[+a] = State[s, a]})#m] =
        new MonadFix[({type m[+a] = State[s, a]})#m] with MonadState[s, ({type m[+a] = State[s, a]})#m]
    {
        // Functor
        private[this] type f[+a] = State[s, a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = State { s =>
            val (a, s_) = runState(m)(s)
            (f(a), s_)
        }
        // Monad
        private[this] type m[+a] = f[a]
        override def `return`[a](a: a): m[a] = State { s => (a, s) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = State { s =>
            val (a, s_) = runState(m)(s)
            runState(k(a))(s_)
        }
        // MonadFix
        override def mfix[a](f: (=> a) => m[a]): m[a] = State { s =>
            lazy val as_ : (a, s) = runState(f(a))(s)
            lazy val (a, s_) = as_
            as_
        }
        // MonadState
        override def get: m[s] = State { s => (s, s) }
        override def put(s: s): m[Unit] = State { _ => ((), s) }
    }
}
