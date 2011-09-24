

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Newtype0[nt, ot, ds <: Kind.MethodList] extends Typeclass with Kind.Newtype0 { outer =>
    override type apply0 = nt
    override type oldtype0 = ot
    override type deriving0 = ds

    final val asNewtype0: Newtype0[nt, ot, ds] = this

    // Core
    //
    type newOf = Lazy[ot] => nt
    def newOf: newOf

    type oldOf = Lazy[nt] => ot
    def oldOf: oldOf

    // Extra
    //
    final def run(nt: nt): ot = oldOf(nt)

    type coNewtype = Newtype0[ot, nt, ds]
    def coNewtype: coNewtype = new Newtype0[ot, nt, ds] {
        override val oldOf: oldOf = ot => outer.newOf(ot)
        override val newOf: newOf = nt => outer.oldOf(nt)
    }
}


trait Newtype0Proxy[nt, ot, ds <: Kind.MethodList] extends Newtype0[nt, ot, ds] {
    def selfNewtype0: Newtype0[nt, ot, ds]

    override def newOf: newOf = selfNewtype0.newOf
    override def oldOf: oldOf = selfNewtype0.oldOf

    override def coNewtype: coNewtype = selfNewtype0.coNewtype
}


object Newtype0 {
    def apply[nt <: Kind.Newtype0](implicit i: Newtype0[nt#apply0, nt#oldtype0, nt#deriving0]): Newtype0[nt#apply0, nt#oldtype0, nt#deriving0] = i
}
