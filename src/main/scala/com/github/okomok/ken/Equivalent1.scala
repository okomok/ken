

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Equivalent1[p[+_], d[+_]] extends Typeclass {
    final val asEquivalent1: Equivalent1[p, d] = this

    // Core
    //
    def imply1[a](p: => p[a]): d[a]
    def unimply1[a](d: => d[a]): p[a]
}


trait Equivalent1Proxy[p[+_], d[+_]] extends Equivalent1[p, d] with Proxy {
    override def self: Equivalent1[p, d]

    override def imply1[a](p: => p[a]): d[a] = self.imply1(p)
    override def unimply1[a](d: => d[a]): p[a] = self.unimply1(d)
}


object Equivalent1 extends Equivalent1Instance {
    def apply[d <: Kind.Function1, p <: Kind.Function1](implicit i: Equivalent1[d#apply, p#apply]): Equivalent1[d#apply, p#apply] = i
}


private[ken] trait Equivalent1Instance { this: Equivalent1.type =>
    implicit def _ofEquivalent1[d[+_], p[+_]](implicit i: Equivalent1[d, p]): Equivalent1[p, d] = new Equivalent1[p, d] {
        override def imply1[a](p: => p[a]): d[a] = i.unimply1(p)
        override def unimply1[a](d: => d[a]): p[a] = i.imply1(d)
    }
}
