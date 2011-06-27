

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] {
    type mtype = m
    def mempty: m
    def mappend(x: m)(y: => m): m
    def mconcat(x: List[m]): m = List.foldr(mappend)(mempty)(x)
}


object Monoid extends MonoidOp with MonoidInstance


trait MonoidOp {
    def mempty[m](implicit i: Monoid[m]): m = i.mempty
    def mappend[m](x: m)(y: => m)(implicit i: Monoid[m]): m = i.mappend(x)(y)
    def mconcat[m](x: List[m])(implicit i: Monoid[m]): m = i.mconcat(x)

    private[ken] class Mappend_[m](x: m)(implicit i: Monoid[m]) {
        def _mappend_(y: => m): m = mappend(x)(y)
    }
    implicit def _mappend_[m](x: m)(implicit i: Monoid[m]): Mappend_[m] = new Mappend_(x)
}


trait MonoidInstance {
    implicit val ofUnit = Unit_

    implicit def ofFunction1[A, b](implicit mb: Monoid[b]): Monoid[A => b] = new Monoid[A => b] {
        private[this] type m = A => b
        override def mempty: m = _ => mb.mempty
        override def mappend(x: m)(y: => m): m = z => mb.mappend(x(z))(y(z))
    }

    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        def mempty: m = (ma.mempty, mb.mempty)
        def mappend(x1: m)(x2: => m): m = (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }
}
