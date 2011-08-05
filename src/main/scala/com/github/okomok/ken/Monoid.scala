

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] extends TypeClass0[m] { outer =>
    final val asMonoid: Monoid[apply] = this

    // Core
    //
    def mempty: m
    def mappend: m => (=> m) => m
    def mconcat: List[m] => m = { x => List.foldr(mappend)(mempty)(x) }

    // Extra
    //
    def dual: Monoid[m] = new Monoid[m] {
        override val mempty: m = outer.mempty
        override val mappend: m => (=> m) => m = x => y => outer.mappend(y)(x)
    }

    // Infix
    //
    sealed class Infix_mappend(x: m) {
        def _mappend_(y: => m): m = mappend(x)(y)
    }
    final implicit def _mappend_(x: m): Infix_mappend = new Infix_mappend(x)
}


trait MonoidProxy[m] extends Monoid[m] with Proxy {
    override def self: Monoid[m]

    override def mempty: m = self.mempty
    override def mappend: m => (=> m) => m = self.mappend
    override def mconcat: List[m] => m = self.mconcat

    override def dual: Monoid[m] = self.dual
}


object Monoid extends MonoidInstance {
    def apply[m](implicit i: Monoid[m]): Monoid[m] = i

/*
    final case class Dual[+a](get: a) extends Strong[a] {
        override def get: a = get
    }

    object Dual {
        implicit def asMonoid[a](implicit i: Monoid[a]): Monoid[Dual[a]] = new Monoid[Dual[a]] {
            override val mempty: m = Dual(i.mempty)
            override val mappend: m => (=> m) => m = x => y => Dual(i.mappend(y.get)(x.get))
        }
    }
*/

    final case class All(override val get: Bool) extends Strong[Bool]

    object All {
        implicit val asWeak: Weak0[All, Bool] = new Weak0[All, Bool] {
            private[this] type p = All
            private[this] type d = Bool
            override def wrap(d: => d): p = All(d)
            override def unwrap(p: p): d = p.get
        }

        implicit val asMonoid: Monoid[All] = new Monoid[All] {
            private[this] type m = All
            override val mempty: m = All(True)
            override val mappend: m => (=> m) => m = x => y => All(x.get && y.get)
        }
    }

    final case class Any_(override val get: Bool) extends Strong[Bool]

    object Any_ {
        implicit val asWeak: Weak0[Any_, Bool] = new Weak0[Any_, Bool] {
            private[this] type p = Any_
            private[this] type d = Bool
            override def wrap(d: => d): p = Any_(d)
            override def unwrap(p: p): d = p.get
        }

        implicit val asMonoid: Monoid[Any_] = new Monoid[Any_] {
            private[this] type m = Any_
            override val mempty: m = Any_(False)
            override val mappend: m => (=> m) => m = x => y => Any_(x.get || y.get)
        }
    }

    final case class Sum[a](override val get: a) extends Strong[a]

    object Sum {
        implicit def asWeak[a]: Weak0[Sum[a], a] = new Weak0[Sum[a], a] {
            private[this] type p = Sum[a]
            private[this] type d = a
            override def wrap(d: => d): p = Sum(d)
            override def unwrap(p: p): d = p.get
        }

        implicit def asMonoid[a](implicit i: Num[a]): Monoid[Sum[a]] = new Monoid[Sum[a]] {
            import i.+
            private[this] type m = Sum[a]
            override val mempty: m = Sum(i.fromInteger(0))
            override val mappend: m => (=> m) => m = x => y => Sum(x.get + y.get)
        }
    }

    final case class Product[a](override val get: a) extends Strong[a]

    object Product {
        implicit def asWeak[a]: Weak0[Product[a], a] = new Weak0[Product[a], a] {
            private[this] type p = Product[a]
            private[this] type d = a
            override def wrap(d: => d): p = Product(d)
            override def unwrap(p: p): d = p.get
        }

        implicit def asMonoid[a](implicit i: Num[a]): Monoid[Product[a]] = new Monoid[Product[a]] {
            import i.*
            private[this] type m = Product[a]
            override val mempty: m = Product(i.fromInteger(1))
            override val mappend: m => (=> m) => m = x => y => Product(x.get * y.get)
        }
    }
}


trait MonoidInstance {
    implicit val ofUnit: Monoid[Unit] = Unit.asMonoid

    implicit def ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = Function.asMonoid[z, b]

    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        override val mempty: m = (ma.mempty, mb.mempty)
        override val mappend: m => (=> m) => m = { x1 => x2 => (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        } }
    }
}
