

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


trait Bounded[a] extends Typeclass0[a] {
    final val asBounded: Bounded[apply0] = this

    // Core
    //
    type minBound = a
    def minBound: minBound

    type maxBound = a
    def maxBound: maxBound
}


trait BoundedProxy[a] extends Bounded[a] {
    def selfBounded: Bounded[a]

    override def minBound: minBound = selfBounded.minBound
    override def maxBound: maxBound = selfBounded.maxBound
}


object Bounded extends BoundedInstance with BoundedShortcut with BoundedDetail {
    def apply[a <: Kind.Function0](implicit i: Bounded[a#apply0]): Bounded[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: Bounded[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0]): Bounded[nt#apply0] = new Bounded[nt#apply0] {
        override val minBound: minBound = j.newOf(i.minBound)
        override val maxBound: maxBound = j.newOf(i.maxBound)
    }

    def weak[nt <: Kind.Newtype0](implicit i: Bounded[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Bounded[nt#oldtype0] = deriving[Kind.dualNewtype0[nt]](i, j.dual)
}


sealed trait BoundedInstance { this: Bounded.type =>
    implicit val ofBool: Bounded[Bool] = _Bool
    implicit val ofChar: Bounded[Char] = Char
    implicit val ofInt: Bounded[Int] = Int
    implicit val ofUnit: Bounded[Unit] = Unit
}


sealed trait BoundedShortcut { this: Bounded.type =>
    def minBound[a](a: a)(implicit i: Bounded[a]): a = i.minBound
    def maxBound[a](a: a)(implicit i: Bounded[a]): a = i.maxBound
}


private[ken] sealed trait BoundedDetail { this: Bounded.type =>
    private[ken] def boundedEnumFrom[a](n: a)(implicit i: Enum[a], j: Bounded[a]): List[a] = {
        List.map(i.toEnum)(Int.enumFromTo(i.fromEnum(n))(i.fromEnum(j.maxBound _asTypeOf_ n)))
    }

    private[ken] def boundedEnumFromThen[a](n1: a)(n2: a)(implicit i: Enum[a], j: Bounded[a]): List[a] = {
        val i_n1 = i.fromEnum(n1)
        val i_n2 = i.fromEnum(n2)
        if (i_n2 > i_n1) {
            List.map(i.toEnum)(Int.enumFromThenTo(i_n1)(i_n2)(i.fromEnum(j.maxBound _asTypeOf_ n1)))
        } else {
            List.map(i.toEnum)(Int.enumFromThenTo(i_n1)(i_n2)(i.fromEnum(j.minBound _asTypeOf_ n1)))
        }
    }
}
