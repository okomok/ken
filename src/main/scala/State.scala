

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class State[s, +a](runState: s => (a, s))


object State {
    def runState[s, a](m: State[s, a]): s => (a, s) = m.runState

    def evalState[s, a](m: State[s, a])(s: s): a = fst(runState(m)(s))

    def execState[s, a](m: State[s, a])(s: s): s = snd(runState(m)(s))

    def mapState[s, a, b](f: (a, s) => (b, s))(m: State[s, a]): State[s, b] = State { f.tupled compose runState(m) }

    def withState[s, a](f: s => s)(m: State[s, a]): State[s, a] = State { runState(m) compose f }

}
