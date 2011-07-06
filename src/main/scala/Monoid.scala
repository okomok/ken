

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] { outer =>
    type apply = m

    def mempty: m
    def mappend(x: m)(y: => m): m
    def mconcat(x: List[m]): m = List.foldr(mappend)(mempty)(x)

    implicit def method(x: m): MonoidMethod[m] = new MonoidMethod[m] {
        override def klass = outer
        override def callee = x
    }
}

trait MonoidMethod[m] extends Method {
    override def klass: Monoid[m]
    override def callee: m
    final def _mappend_(y: => m): m = klass.mappend(callee)(y)
}

trait MonoidProxy[m] extends Monoid[m] with Proxy {
    override def self: Monoid[m]
    override def mempty: m = self.mempty
    override def mappend(x: m)(y: => m): m = self.mappend(x)(y)
    override def mconcat(x: List[m]): m = self.mconcat(x)
}


object Monoid extends MonoidInstance {
    def apply[m](implicit i: Monoid[m]): Monoid[m] = i
}

trait MonoidInstance {
    implicit val ofUnit = Unit_

    implicit def ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = new Monoid[z => b] {
        private[this] type m = z => b
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
