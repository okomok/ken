

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Endo[a](override val get: a => a) extends Strong[a => a]


object Endo {
    sealed trait apply[a] extends Kind.Strong0 {
        override type apply = Endo[a]
        override type weak = a => a
    }

    implicit def weak[a]: Weak0[Endo[a], a => a] = new Weak0[Endo[a], a => a] {
        private[this] type p = Endo[a]
        private[this] type d = a => a
        override def wrap(d: => d): p = Endo(d)
        override def unwrap(p: p): d = p.run

    }

    implicit def asMonoid[a]: Monoid[Endo[a]] = new Monoid[Endo[a]] {
        private[this] type m = Endo[a]
        override val mempty: m = Endo(id[a])
        override val mappend: m => (=> m) => m = x => y => Endo(x.run compose y.run)
    }
}
