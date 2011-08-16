

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Newtype2[nt[-_, +_], ot[-_, +_]] extends Typeclass with Kind.AbstractNewtype2 { outer =>
    override type apply2[-a, +b] = nt[a, b]
    override type oldtype2[-a, +b] = ot[a, b]

    final val asNewtype2: Newtype2[nt, ot] = this

    // Core
    //
    def newOf[a, b](ot: => ot[a, b]): nt[a, b]
    def oldOf[a, b](nt: => nt[a, b]): ot[a, b]

    // Extra
    //
    final def run[a, b](nt: nt[a, b]): ot[a, b] = oldOf(nt)

    def dual: Newtype2[ot, nt] = new Newtype2[ot, nt] {
        override def oldOf[a, b](ot: => ot[a, b]): nt[a, b] = outer.newOf(ot)
        override def newOf[a, b](nt: => nt[a, b]): ot[a, b] = outer.oldOf(nt)
    }
}


trait Newtype2Proxy[nt[-_, +_], ot[-_, +_]] extends Newtype2[nt, ot] with Proxy {
    override def self: Newtype2[nt, ot]

    override def newOf[a, b](ot: => ot[a, b]): nt[a, b] = self.newOf(ot)
    override def oldOf[a, b](nt: => nt[a, b]): ot[a, b] = self.oldOf(nt)

    override def dual: Newtype2[ot, nt] = self.dual
}


object Newtype2 {
    def apply[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: Newtype2[nt#apply2, ot#apply2]): Newtype2[nt#apply2, ot#apply2] = i
}
