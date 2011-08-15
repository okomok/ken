

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Newtype0[nt, ot] extends Typeclass with Kind.AbstractNewtype0 { outer =>
    override type apply0 = nt
    override type oldtype0 = ot

    final val asNewtype0: Newtype0[nt, ot] = this

    // Core
    //
    def new0(ot: => ot): nt
    def old0(nt: => nt): ot

    // Extra
    //
    final def run(nt: nt): ot = old0(nt)

    def dual: Newtype0[ot, nt] = new Newtype0[ot, nt] {
        override def old0(ot: => ot): nt = outer.new0(ot)
        override def new0(nt: => nt): ot = outer.old0(nt)
    }
}


trait Newtype0Proxy[nt, ot] extends Newtype0[nt, ot] with Proxy {
    override def self: Newtype0[nt, ot]

    override def new0(ot: => ot): nt = self.new0(ot)
    override def old0(nt: => nt): ot = self.old0(nt)

    override def dual: Newtype0[ot, nt] = self.dual
}


object Newtype0 {
    def apply[nt, ot](implicit i: Newtype0[nt, ot]): Newtype0[nt, ot] = i
}
