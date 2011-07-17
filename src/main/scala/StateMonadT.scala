

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait StateMonadT[s, m[+_], +a] extends Up[StateMonadT[s, m, a]] {
    def run(s: s): m[(a, s)]
}


trait StateMonadTProxy[s, m[+_], +a] extends StateMonadT[s, m, a] with Proxy {
    override def self: StateMonadT[s, m, a]
    override def run(s: s): m[(a, s)] = self.run(s)
}


object StateMonadT {
    def apply[s, m[+_], a](_run: s => m[(a, s)]): StateMonadT[s, m, a] = new StateMonadT[s, m, a] {
        override def run(s: s): m[(a, s)] = _run(s)
    }
}
