

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] {
    type mtype = m
    def mempty: m
    def mappend(x: m)(y: m): m
    def mconcat(x: List[m]): m = Prelude.foldr[m, m](x => y => mappend(x)(y()))(mempty)(x)

    private[ken] class Mappend_(x: m) {
        def _mappend_(y: m): m = mappend(x)(y)
    }
    implicit def _mappend_(x: m): Mappend_ = new Mappend_(x)
}


object Monoid {
    implicit val ofUnit = Unit_

    implicit def ofFunction1[A, b](implicit mb: Monoid[b]): Monoid[A => b] = new Monoid[A => b] {
        private[this] type m = A => b
        override def mempty: m = _ => mb.mempty
        override def mappend(x: m)(y: m): m = z => mb.mappend(x(z))(y(z))
    }

    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        def mempty: m = (ma.mempty, mb.mempty)
        def mappend(x1: m)(x2: m): m = (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }
}
