

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Endo[a](override val get: a => a) extends NewtypeOf[a => a] with Kind.AbstractNewtype0 {
    override type apply0 = Endo[a]
    override type oldtype0 = a => a
}


object Endo {
    implicit def _asNewtype0[a]: Newtype0[Endo[a], a => a] = new Newtype0[Endo[a], a => a] {
        private[this] type nt = Endo[a]
        private[this] type ot = a => a
        override def newOf(ot: => ot): nt = Endo(ot)
        override def oldOf(nt: => nt): ot = nt.run

    }

    implicit def _asMonoid[a]: Monoid[Endo[a]] = new Monoid[Endo[a]] {
        private[this] type m = Endo[a]
        override val mempty: m = Endo(id[a])
        override val mappend: m => (=> m) => m = x => y => Endo(x.run compose y.run)
    }
}
