

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] extends Typeclass0[m] { outer =>
    final val asMonoid: Monoid[apply0] = this

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

    // Newtypes
    //
    override def deriving[nt](implicit i: Newtype0[nt, m]): Monoid[nt] = new Monoid[nt] {
        private[this] type _m = nt
        override val mempty: _m = i.newOf(outer.mempty)
        override val mappend: _m => (=> _m) => _m = x => y => i.newOf(outer.mappend(i.oldOf(x))(i.oldOf(y)))
        override val mconcat: List[_m] => _m = xs => i.newOf(outer.mconcat(List.map[_m, m](Function.!(i.oldOf))(xs)))
    }
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

    def weak[nt <: Kind.Newtype0](implicit i: Monoid[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Monoid[nt#oldtype0] = i.deriving[nt#oldtype0](j.dual)

    // Dual
    //
    final case class Dual[+a](override val get: a) extends NewtypeOf[a]

    object Dual {
        implicit def _asNewtype0[a]: Newtype0[Dual[a], a] = new Newtype0[Dual[a], a] {
            private[this] type nt = Dual[a]
            private[this] type ot = a
            override def newOf(ot: => ot): nt = Dual(ot)
            override def oldOf(nt: => nt): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Monoid[a]): Monoid[Dual[a]] = new Monoid[Dual[a]] {
            private[this] type m = Dual[a]
            override val mempty: m = Dual(i.mempty)
            override val mappend: m => (=> m) => m = x => y => Dual(i.mappend(y.get)(x.get))
        }
    }

    // All
    //
    final case class All(override val get: Bool) extends NewtypeOf[Bool]

    object All extends Newtype0[All, Bool] with ThisIsInstance {
        // Overrides
        //
        // Newtype0
        private[this] type nt = All
        private[this] type ot = Bool
        override def newOf(ot: => ot): nt = All(ot)
        override def oldOf(nt: => nt): ot = nt.get

        implicit val _asMonoid: Monoid[All] = new Monoid[All] {
            private[this] type m = All
            override val mempty: m = All(True)
            override val mappend: m => (=> m) => m = x => y => All(x.get && y.get)
        }
    }

    // Any_
    //
    final case class Any_(override val get: Bool) extends NewtypeOf[Bool]

    object Any_  extends Newtype0[Any_, Bool] with ThisIsInstance {
        // Overrrides
        //
        // Newtype0
        private[this] type nt = Any_
        private[this] type ot = Bool
        override def newOf(ot: => ot): nt = Any_(ot)
        override def oldOf(nt: => nt): ot = nt.get

        implicit val _asMonoid: Monoid[Any_] = new Monoid[Any_] {
            private[this] type m = Any_
            override val mempty: m = Any_(False)
            override val mappend: m => (=> m) => m = x => y => Any_(x.get || y.get)
        }
    }

    // Sum
    //
    final case class Sum[a](override val get: a) extends NewtypeOf[a] with Kind.AbstractNewtype0 {
        override type apply0 = Sum[a]
        override type oldtype0 = a
    }

    object Sum {
        implicit def _asNewtype0[a]: Newtype0[Sum[a], a] = new Newtype0[Sum[a], a] {
            private[this] type nt = Sum[a]
            private[this] type ot = a
            override def newOf(ot: => ot): nt = Sum(ot)
            override def oldOf(nt: => nt): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Num[a]): Monoid[Sum[a]] = new Monoid[Sum[a]] {
            import i.+
            private[this] type m = Sum[a]
            override val mempty: m = Sum(i.fromInteger(0))
            override val mappend: m => (=> m) => m = x => y => Sum(x.get + y.get)
        }
    }

    // Product
    //
    final case class Product[a](override val get: a) extends NewtypeOf[a] with Kind.AbstractNewtype0 {
        override type apply0 = Product[a]
        override type oldtype0 = a
    }

    object Product {
        implicit def _asNewtype0[a]: Newtype0[Product[a], a] = new Newtype0[Product[a], a] {
            private[this] type nt = Product[a]
            private[this] type ot = a
            override def newOf(ot: => ot): nt = Product(ot)
            override def oldOf(nt: => nt): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Num[a]): Monoid[Product[a]] = new Monoid[Product[a]] {
            import i.*
            private[this] type m = Product[a]
            override val mempty: m = Product(i.fromInteger(1))
            override val mappend: m => (=> m) => m = x => y => Product(x.get * y.get)
        }
    }
}


trait MonoidInstance {
    implicit val _ofUnit: Monoid[Unit] = Unit
    implicit def _ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = Function._asMonoid[z, b]
    implicit def _ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = Pair._asMonoid[a, b]
}
