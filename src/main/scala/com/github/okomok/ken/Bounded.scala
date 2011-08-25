

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
    def minBound: a
    def maxBound: a
}


trait BoundedProxy[a] extends Bounded[a] {
    def selfBounded: Bounded[a]

    override def minBound: a = selfBounded.minBound
    override def maxBound: a = selfBounded.maxBound
}


object Bounded extends BoundedInstance {
    def apply[a <: Kind.Function0](implicit i: Bounded[a#apply0]): Bounded[a#apply0] = i

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


sealed trait BoundedInstance { this: Bounded.type =>
    implicit val _ofBool: Bounded[Bool] = _Bool
    implicit val _ofChar: Bounded[Char] = Char
    implicit val _ofInt: Bounded[Int] = Int
    implicit val _ofUnit: Bounded[Unit] = Unit
}
