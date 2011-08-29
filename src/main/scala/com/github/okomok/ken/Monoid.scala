

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Monoid[m] extends Typeclass0[m] { outer =>
    final val asMonoid: Monoid[apply0] = this

    // Core
    //
    def mempty: m
    def mappend: m => Lazy[m] => m
    def mconcat: List[m] => m = { x => List.foldr(mappend)(mempty)(x) }

    // Extra
    //
    def dual: Monoid[m] = new Monoid[m] {
        override val mempty: m = outer.mempty
        override val mappend: m => Lazy[m] => m = x => y => outer.mappend(y.!)(x)
    }

    // Operators
    //
    sealed class Op_mappend(x: m) {
        def _mappend_(y: Lazy[m]): m = mappend(x)(y)
    }
    final implicit def _mappend_(x: m): Op_mappend = new Op_mappend(x)
}


trait MonoidProxy[m] extends Monoid[m] {
    def selfMonoid: Monoid[m]

    override def mempty: m = selfMonoid.mempty
    override def mappend: m => Lazy[m] => m = selfMonoid.mappend
    override def mconcat: List[m] => m = selfMonoid.mconcat

    override def dual: Monoid[m] = selfMonoid.dual
}


object Monoid extends MonoidInstance {
    def apply[m <: Kind.Function0](implicit i: Monoid[m#apply0]): Monoid[m#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Monoid[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Monoid[nt#apply0] = new Monoid[nt#apply0] {
        private[this] type m = nt#apply0
        override val mempty: m = j.newOf(i.mempty)
        override val mappend: m => Lazy[m] => m = x => y => j.newOf(i.mappend(j.oldOf(x))(j.oldOf(y)))
        override val mconcat: List[m] => m = xs => j.newOf(i.mconcat(List.map[m, ot#apply0](j.oldOf)(xs)))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Monoid[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Monoid[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)

    // Dual
    //
    final case class Dual[+a](override val get: a) extends NewtypeOf[a]

    object Dual {
        implicit def _asNewtype0[a]: Newtype0[Dual[a], a] = new Newtype0[Dual[a], a] {
            private[this] type nt = Dual[a]
            private[this] type ot = a
            override def newOf(ot: Lazy[ot]): nt = Dual(ot)
            override def oldOf(nt: Lazy[nt]): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Monoid[a]): Monoid[Dual[a]] = new Monoid[Dual[a]] {
            private[this] type m = Dual[a]
            override val mempty: m = Dual(i.mempty)
            override val mappend: m => Lazy[m] => m = x => y => Dual(i.mappend(y.get)(x.get))
        }
    }

    // All
    //
    final case class All(override val get: Bool) extends NewtypeOf[Bool]

    object All extends Newtype0[All, Bool] with ThisIsInstance {
        // Overrides
        //
        // Newtype0
        private type nt = All
        private type ot = Bool
        override def newOf(ot: Lazy[ot]): nt = All(ot)
        override def oldOf(nt: Lazy[nt]): ot = nt.get

        implicit val _asMonoid: Monoid[All] = new Monoid[All] {
            private[this] type m = All
            override val mempty: m = All(True)
            override val mappend: m => Lazy[m] => m = x => y => All(x.get && y.get)
        }
    }

    // Any_
    //
    final case class Any_(override val get: Bool) extends NewtypeOf[Bool]

    object Any_  extends Newtype0[Any_, Bool] with ThisIsInstance {
        // Overrrides
        //
        // Newtype0
        private type nt = Any_
        private type ot = Bool
        override def newOf(ot: Lazy[ot]): nt = Any_(ot)
        override def oldOf(nt: Lazy[nt]): ot = nt.get

        implicit val _asMonoid: Monoid[Any_] = new Monoid[Any_] {
            private[this] type m = Any_
            override val mempty: m = Any_(False)
            override val mappend: m => Lazy[m] => m = x => y => Any_(x.get || y.get)
        }
    }

    // Sum
    //
    final case class Sum[a](override val get: a) extends NewtypeOf[a] with Kind.AbstractNewtype0 {
        override type oldtype0 = a
    }

    object Sum {
        implicit def _asNewtype0[a]: Newtype0[Sum[a], a] = new Newtype0[Sum[a], a] {
            private[this] type nt = Sum[a]
            private[this] type ot = a
            override def newOf(ot: Lazy[ot]): nt = Sum(ot)
            override def oldOf(nt: Lazy[nt]): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Num[a]): Monoid[Sum[a]] = new Monoid[Sum[a]] {
            import i.+
            private[this] type m = Sum[a]
            override val mempty: m = Sum(i.fromIntegral(0))
            override val mappend: m => Lazy[m] => m = x => y => Sum(x.get + y.get)
        }
    }

    // Product
    //
    final case class Product[a](override val get: a) extends NewtypeOf[a] with Kind.AbstractNewtype0 {
        override type oldtype0 = a
    }

    object Product {
        implicit def _asNewtype0[a]: Newtype0[Product[a], a] = new Newtype0[Product[a], a] {
            private[this] type nt = Product[a]
            private[this] type ot = a
            override def newOf(ot: Lazy[ot]): nt = Product(ot)
            override def oldOf(nt: Lazy[nt]): ot = nt.get
        }

        implicit def _asMonoid[a](implicit i: Num[a]): Monoid[Product[a]] = new Monoid[Product[a]] {
            import i.*
            private[this] type m = Product[a]
            override val mempty: m = Product(i.fromIntegral(1))
            override val mappend: m => Lazy[m] => m = x => y => Product(x.get * y.get)
        }
    }
}


trait MonoidInstance {
    implicit val ofUnit: Monoid[Unit] = Unit
    implicit def ofFunction1[z, b](implicit mb: Monoid[b]): Monoid[z => b] = Function._asMonoid[z, b]
    implicit def ofPair[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = Pair._asMonoid[a, b]
}
