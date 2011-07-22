

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait WriterMonadT[w, m[+_], +a] extends Up[WriterMonadT[w, m, a]] {
    def monoid: Monoid[w]
    def run: m[(a, w)]
}


trait WriterMonadTProxy[w, m[+_], +a] extends WriterMonadT[w, m, a] with Proxy {
    override def self: WriterMonadT[w, m, a]
    override def monoid: Monoid[w] = self.monoid
    override def run: m[(a, w)] = self.run
}


object WriterMonadT {
    def apply[w, m[+_], a](_run: => m[(a, w)])(implicit i: Monoid[w]): WriterMonadT[w, m, a] = new WriterMonadT[w, m, a] {
        override def monoid: Monoid[w] = i
        override def run: m[(a, w)] = _run
    }
}
