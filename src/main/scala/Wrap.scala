

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Wrap[+a] {
    def run: a
}


trait WrapProxy[+a] extends Wrap[a] with Proxy {
    override def self: Wrap[a]
    override def run: a = self.run
}


object Wrap {
    def apply[a](a: a): Wrap[a] = new Wrap[a] {
        override def run: a = a
    }
}
