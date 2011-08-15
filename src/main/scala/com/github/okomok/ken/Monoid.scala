

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

    override def deriving[nt](implicit i: Newtype0[nt, m]): Monoid[nt] = new Monoid[nt] {
        private[this] type _m = nt
        override val mempty: _m = i.new0(outer.mempty)
        override val mappend: _m => (=> _m) => _m = x => y => i.new0(outer.mappend(i.old0(x))(i.old0(y)))
        override val mconcat: List[_m] => _m = xs => i.new0(outer.mconcat(List.map[_m, m](Function.!(i.old0))(xs)))
    }

    override def weak[ot](implicit i: Newtype0[m, ot]): Monoid[ot] = deriving[ot](i.dual)
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

    // Dual
    //
    final case class Dual[+a](override val get: a) extends NewtypeOf[a]

    object Dual {
        implicit def weak[a]: Imply0[Dual[a], a] = new Imply0[Dual[a], a] {
            private[this] type p = Dual[a]
            private[this] type d = a
            override def imply0(p: p): d = p.get
            override def unimply0(d: => d): p = Dual(d)
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

    object All {
        implicit val weak: Imply0[All, Bool] = new Imply0[All, Bool] {
            private[this] type p = All
            private[this] type d = Bool
            override def imply0(p: p): d = p.get
            override def unimply0(d: => d): p = All(d)
        }

        implicit val _asMonoid: Monoid[All] = new Monoid[All] {
            private[this] type m = All
            override val mempty: m = All(True)
            override val mappend: m => (=> m) => m = x => y => All(x.get && y.get)
        }
    }

    // Any_
    //
    final case class Any_(override val get: Bool) extends NewtypeOf[Bool]

    object Any_ {
        implicit val weak: Imply0[Any_, Bool] = new Imply0[Any_, Bool] {
            private[this] type p = Any_
            private[this] type d = Bool
            override def imply0(p: p): d = p.get
            override def unimply0(d: => d): p = Any_(d)
        }

        implicit val _asMonoid: Monoid[Any_] = new Monoid[Any_] {
            private[this] type m = Any_
            override val mempty: m = Any_(False)
            override val mappend: m => (=> m) => m = x => y => Any_(x.get || y.get)
        }
    }

    // Sum
    //
    final case class Sum[a](override val get: a) extends NewtypeOf[a]

    object Sum {
        implicit def weak[a]: Imply0[Sum[a], a] = new Imply0[Sum[a], a] {
            private[this] type p = Sum[a]
            private[this] type d = a
            override def imply0(p: p): d = p.get
            override def unimply0(d: => d): p = Sum(d)
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
    final case class Product[a](override val get: a) extends NewtypeOf[a]

    object Product {
        implicit def weak[a]: Imply0[Product[a], a] = new Imply0[Product[a], a] {
            private[this] type p = Product[a]
            private[this] type d = a
            override def imply0(p: p): d = p.get
            override def unimply0(d: => d): p = Product(d)
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
