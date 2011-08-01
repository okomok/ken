

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] extends Klass { outer =>
    type apply = m
    final def asMonoid: Monoid[m] = this

// Overridables
    def mempty: m
    def mappend: m => (=> m) => m
    def mconcat: List[m] => m = { x => List.foldr(mappend)(mempty)(x) }

// Utilities
    final def dual: Monoid[m] = new Monoid[m] {
        override val mempty: m = outer.mempty
        override val mappend: m => (=> m) => m = x => y => outer.mappend(y)(x)
    }

// Infix Operators
    sealed class Infix_mappend(x: m) {
        def _mappend(y: => m): m = mappend(x)(y)
    }
    final implicit def _mappend_(x: m): Infix_mappend = new Infix_mappend(x)
}


trait MonoidProxy[m] extends Monoid[m] with Proxy {
    override def self: Monoid[m]
    override def mempty: m = self.mempty
    override def mappend: m => (=> m) => m = self.mappend
    override def mconcat: List[m] => m = self.mconcat
}


object Monoid extends MonoidInstance {
    def apply[m](implicit i: Monoid[m]): Monoid[m] = i

/*
    final case class Dual[+a](get: a) extends Identity[a] {
        override def run: a = get
    }

    object Dual {
        implicit def monoid[a](implicit i: Monoid[a]): Monoid[Dual[a]] = new Monoid[Dual[a]] {
            override val mempty: m = Dual(i.mempty)
            override val mappend: m => (=> m) => m = x => y => Dual(i.mappend(y.get)(x.get))
        }
    }
*/
}


trait MonoidInstance {
    implicit val ofUnit: Monoid[Unit] = Unit.monoid

    implicit def ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = Function.monoid[z, b]

    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        override val mempty: m = (ma.mempty, mb.mempty)
        override val mappend: m => (=> m) => m = { x1 => x2 => (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        } }
    }
}
