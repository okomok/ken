

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Newtype0[nt, ot] extends Typeclass with Kind.Newtype0 { outer =>
    override type apply0 = nt
    override type oldtype0 = ot

    final val asNewtype0: Newtype0[nt, ot] = this

    // Core
    //
    type newOf = Lazy[ot] => nt
    def newOf: newOf

    type oldOf = Lazy[nt] => ot
    def oldOf: oldOf

    // Extra
    //
    final def run(nt: nt): ot = oldOf(nt)

    type coNewtype = Newtype0[ot, nt]
    def coNewtype: coNewtype = new Newtype0[ot, nt] {
        override val oldOf: oldOf = ot => outer.newOf(ot)
        override val newOf: newOf = nt => outer.oldOf(nt)
    }
}


trait Newtype0Proxy[nt, ot] extends Newtype0[nt, ot] {
    def selfNewtype0: Newtype0[nt, ot]

    override def newOf: newOf = selfNewtype0.newOf
    override def oldOf: oldOf = selfNewtype0.oldOf

    override def coNewtype: coNewtype = selfNewtype0.coNewtype
}


object Newtype0 {
    def apply[nt, ot](implicit i: Newtype0[nt, ot]): Newtype0[nt, ot] = i
}
