

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Newtype1[nt[+_], ot[+_]] extends Typeclass with Kind.Newtype1 { outer =>
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

    def coNewtype: Newtype1[ot, nt] = new Newtype1[ot, nt] {
        override def oldOf[a](ot: Lazy[ot[a]]): nt[a] = outer.newOf(ot)
        override def newOf[a](nt: Lazy[nt[a]]): ot[a] = outer.oldOf(nt)
    }
}


trait Newtype1Proxy[nt[+_], ot[+_]] extends Newtype1[nt, ot] {
    def selfNewtype1: Newtype1[nt, ot]

    override def newOf[a](ot: Lazy[ot[a]]): nt[a] = selfNewtype1.newOf(ot)
    override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = selfNewtype1.oldOf(nt)

    override def coNewtype: Newtype1[ot, nt] = selfNewtype1.coNewtype
}


object Newtype1 extends Newtype1Instance {
    def apply[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Newtype1[nt#apply, ot#apply]): Newtype1[nt#apply, ot#apply] = i
}


sealed trait Newtype1Instance { this: Newtype1.type =>
    implicit def ofMonadT[m[+_], n[+_], u[+_]](implicit i: MonadT[m, n, u]): Newtype1[m, ({type ot[+a] = n[u[a]]})#ot] = new Newtype1[m, ({type ot[+a] = n[u[a]]})#ot] {
        private type nt[+a] = m[a]
        private type ot[+a] = n[u[a]]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = i.newOf(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = i.oldOf(nt)
    }
}
