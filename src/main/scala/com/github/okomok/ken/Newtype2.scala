

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// May be removed.


trait Newtype2[nt[-_, +_], ot[-_, +_]] extends TypeclassLike with Kind.Newtype2 { outer =>
    override type apply2[-a, +b] = nt[a, b]
    override type oldtype2[-a, +b] = ot[a, b]

    final val asNewtype2: Newtype2[nt, ot] = this

    // Core
    //
    def newOf[a, b](ot: Lazy[ot[a, b]]): nt[a, b]
    def oldOf[a, b](nt: Lazy[nt[a, b]]): ot[a, b]

    // Extra
    //
    final def run[a, b](nt: nt[a, b]): ot[a, b] = oldOf(nt)

    def coNewtype: Newtype2[ot, nt] = new Newtype2[ot, nt] {
        override def oldOf[a, b](ot: Lazy[ot[a, b]]): nt[a, b] = outer.newOf(ot)
        override def newOf[a, b](nt: Lazy[nt[a, b]]): ot[a, b] = outer.oldOf(nt)
    }
}


trait Newtype2Proxy[nt[-_, +_], ot[-_, +_]] extends Newtype2[nt, ot] {
    type selfNewtype2 = Newtype2[nt, ot]
    def selfNewtype2: selfNewtype2

    override def newOf[a, b](ot: Lazy[ot[a, b]]): nt[a, b] = selfNewtype2.newOf(ot)
    override def oldOf[a, b](nt: Lazy[nt[a, b]]): ot[a, b] = selfNewtype2.oldOf(nt)

    override def coNewtype: Newtype2[ot, nt] = selfNewtype2.coNewtype
}


object Newtype2 {
    def apply[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: Newtype2[nt#apply2, ot#apply2]): Newtype2[nt#apply2, ot#apply2] = i
}
