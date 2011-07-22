

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ReaderMonadT[r, m[+_], +a] extends Up[ReaderMonadT[r, m, a]] {
    def run(r: r): m[a]
}


trait ReaderMonadTProxy[r, m[+_], +a] extends ReaderMonadT[r, m, a] with Proxy {
    override def self: ReaderMonadT[r, m, a]
    override def run(r: r): m[a] = self.run(r)
}


object ReaderMonadT {
    def apply[r, m[+_], a](_run: r => m[a]): ReaderMonadT[r, m, a] = new ReaderMonadT[r, m, a] {
        override def run(r: r): m[a] = _run(r)
    }
}
