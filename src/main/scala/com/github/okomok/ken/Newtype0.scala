

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


object Newtype0 extends Newtype0Instance {
    def apply[nt <: Kind.Newtype0](implicit i: Newtype0[nt#apply0, nt#oldtype0, nt#deriving0]): Newtype0[nt#apply0, nt#oldtype0, nt#deriving0] = i
}


sealed trait Newtype0Instance { this: Newtype0.type =>
    implicit def ofNewtype1[nt[+_], ot[+_], ds <: Kind.MethodList, a](implicit i: Newtype1[nt, ot]): Newtype0[nt[a], ot[a], ds] = new Newtype0[nt[a], ot[a], ds] {
        override val newOf: newOf = ot => i.newOf(ot)
        override val oldOf: oldOf = nt => i.oldOf(nt)
    }

    implicit def ofNewtype2[nt[-_, +_], ot[-_, +_], ds <: Kind.MethodList, a, b](implicit i: Newtype2[nt, ot]): Newtype0[nt[a, b], ot[a, b], ds] = new Newtype0[nt[a, b], ot[a, b], ds] {
        override val newOf: newOf = ot => i.newOf(ot)
        override val oldOf: oldOf = nt => i.oldOf(nt)
    }
}
