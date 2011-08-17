

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Newtype1[nt[+_], ot[+_]] extends Typeclass with Kind.AbstractNewtype1 { outer =>
    override type apply1[+a] = nt[a]
    override type oldtype1[+a] = ot[a]

    final val asNewtype1: Newtype1[nt, ot] = this

    // Core
    //
    def newOf[a](ot: Lazy[ot[a]]): nt[a]
    def oldOf[a](nt: Lazy[nt[a]]): ot[a]

    // Extra
    //
    final def run[a](nt: nt[a]): ot[a] = oldOf(nt)

    def dual: Newtype1[ot, nt] = new Newtype1[ot, nt] {
        override def oldOf[a](ot: Lazy[ot[a]]): nt[a] = outer.newOf(ot)
        override def newOf[a](nt: Lazy[nt[a]]): ot[a] = outer.oldOf(nt)
    }
}


trait Newtype1Proxy[nt[+_], ot[+_]] extends Newtype1[nt, ot] with Proxy {
    override def self: Newtype1[nt, ot]

    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = self.newOf(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = self.oldOf(nt)

    override def dual: Newtype1[ot, nt] = self.dual
}


object Newtype1 {
    def apply[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Newtype1[nt#apply, ot#apply]): Newtype1[nt#apply, ot#apply] = i
}
