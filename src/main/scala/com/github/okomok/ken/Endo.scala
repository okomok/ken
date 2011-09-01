

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Endo[a](override val get: a => a) extends NewtypeOf[a => a] with Kind.AbstractNewtype0 {
    override type oldtype0 = a => a
}


object Endo {
    implicit def _asNewtype0[a]: Newtype0[Endo[a], a => a] = new Newtype0[Endo[a], a => a] {
        override val newOf: newOf = ot => Endo(ot)
        override val oldOf: oldOf = nt => nt.run

    }

    implicit def _asMonoid[a]: Monoid[Endo[a]] = new Monoid[Endo[a]] {
        override val mempty: mempty = Endo(id[a])
        override val mappend: mappend = x => y => Endo(x.run compose y.!.run)
    }
}
