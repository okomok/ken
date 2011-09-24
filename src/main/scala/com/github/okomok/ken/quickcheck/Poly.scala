

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


object Poly {
    private val _ia = Arbitary.ofInteger
    private val _ic = CoArbitary.ofInteger

    // A
    //
    final case class A(override val get: Integer) extends Strong[Integer]

    object A extends Newtype0[A, Integer]
        with Arbitary[A] with CoArbitary[A] with Eq.Of[A] with Show.Of[A] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => A(ot)
        override val oldOf: oldOf = nt => nt.get
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => A(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case A(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield A(x_) }
        // CoArbitary
        type a = A
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unA(a))(gen)

        val unA: A => Integer = a => a.get
    }

    // B
    //
    final case class B(override val get: Integer) extends Strong[Integer]

    object B extends Newtype0[B, Integer]
        with Arbitary[B] with CoArbitary[B] with Eq.Of[B] with Show.Of[B] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => B(ot)
        override val oldOf: oldOf = nt => nt.get
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => B(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case B(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield B(x_) }
        // CoArbitary
        type a = B
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unB(a))(gen)

        val unB: B => Integer = a => a.get
    }

    // C
    //
    final case class C(override val get: Integer) extends Strong[Integer]

    object C extends Newtype0[C, Integer]
        with Arbitary[C] with CoArbitary[C] with Eq.Of[C] with Show.Of[C] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => C(ot)
        override val oldOf: oldOf = nt => nt.get
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => C(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case C(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield C(x_) }
        // CoArbitary
        type a = C
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unC(a))(gen)

        val unC: C => Integer = a => a.get
    }

    // OrdA
    //
    final case class OrdA(override val get: Integer) extends Strong[Integer]

    object OrdA extends Newtype0[OrdA, Integer]
        with Arbitary[OrdA] with CoArbitary[OrdA] with OrdProxy[OrdA] with Show.Of[OrdA] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => OrdA(ot)
        override val oldOf: oldOf = nt => nt.get
        // Ord
        override val selfOrd = Ord.deriving[OrdA.type]
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => OrdA(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case OrdA(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield OrdA(x_) }
        // OrdAoArbitary
        type a = OrdA
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unOrdA(a))(gen)

        val unOrdA: OrdA => Integer = a => a.get
    }

    // OrdB
    //
    final case class OrdB(override val get: Integer) extends Strong[Integer]

    object OrdB extends Newtype0[OrdB, Integer]
        with Arbitary[OrdB] with CoArbitary[OrdB] with OrdProxy[OrdB] with Show.Of[OrdB] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => OrdB(ot)
        override val oldOf: oldOf = nt => nt.get
        // Ord
        override val selfOrd = Ord.deriving[OrdB.type]
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => OrdB(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case OrdB(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield OrdB(x_) }
        // OrdBoArbitary
        type a = OrdB
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unOrdB(a))(gen)

        val unOrdB: OrdB => Integer = a => a.get
    }

    // OrdC
    //
    final case class OrdC(override val get: Integer) extends Strong[Integer]

    object OrdC extends Newtype0[OrdC, Integer]
        with Arbitary[OrdC] with CoArbitary[OrdC] with OrdProxy[OrdC] with Show.Of[OrdC] with ThisIsInstance
    {
        // Overrides
        //
        // Newtype0
        override val newOf: newOf = ot => OrdC(ot)
        override val oldOf: oldOf = nt => nt.get
        // Ord
        override val selfOrd = Ord.deriving[OrdC.type]
        // Arbitary
        override def arbitary: arbitary = Gen.fmap((x: Integer) => OrdC(Integer.abs(x) + 1))(_ia.arbitary)
        override def shrink: shrink = { case OrdC(x) => for { x_ <- _ia.shrink(x) if x_ > 0 } yield OrdC(x_) }
        // OrdCoArbitary
        type a = OrdC
        override def coarbitary[c](a: a)(gen: Gen[c]): Gen[c] = _ic.coarbitary(unOrdC(a))(gen)

        val unOrdC: OrdC => Integer = a => a.get
    }
}
