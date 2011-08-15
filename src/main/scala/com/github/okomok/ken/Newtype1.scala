

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
    def new1[a](ot: => ot[a]): nt[a]
    def old1[a](nt: => nt[a]): ot[a]

    // Extra
    //
    final def run[a](nt: nt[a]): ot[a] = old1(nt)

    def dual: Newtype1[ot, nt] = new Newtype1[ot, nt] {
        override def old1[a](ot: => ot[a]): nt[a] = outer.new1(ot)
        override def new1[a](nt: => nt[a]): ot[a] = outer.old1(nt)
    }
}


trait Newtype1Proxy[nt[+_], ot[+_]] extends Newtype1[nt, ot] with Proxy {
    override def self: Newtype1[nt, ot]

    override def new1[a](ot: => ot[a]): nt[a] = self.new1(ot)
    override def old1[a](nt: => nt[a]): ot[a] = self.old1(nt)

    override def dual: Newtype1[ot, nt] = self.dual
}


object Newtype1 {
    def apply[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Newtype1[nt#apply, ot#apply]): Newtype1[nt#apply, ot#apply] = i
}
