

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] extends Klass {
    type apply = m

// Overridables
    def mempty: m
    def mappend(x: m)(y: => m): m
    def mconcat(x: List[m]): m = List.foldr(mappend)(mempty)(x)

// Infix Operators
    sealed class Infix_mappend(x: m) {
        def _mappend(y: => m): m = mappend(x)(y)
    }
    final implicit def _mappend_(x: m): Infix_mappend = new Infix_mappend(x)
}


trait MonoidProxy[m] extends Monoid[m] with Proxy {
    override def self: Monoid[m]
    override def mempty: m = self.mempty
    override def mappend(x: m)(y: => m): m = self.mappend(x)(y)
    override def mconcat(x: List[m]): m = self.mconcat(x)
}


object Monoid {
    def apply[m](implicit i: Monoid[m]): Monoid[m] = i

    implicit val ofUnit = Unit.monoid

    implicit def ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = Function.monoid[z, b]

    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        def mempty: m = (ma.mempty, mb.mempty)
        def mappend(x1: m)(x2: => m): m = (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }
}
