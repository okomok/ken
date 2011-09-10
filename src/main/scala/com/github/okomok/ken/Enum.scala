

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


trait Enum[a] extends Typeclass0[a] {
    final val asEnum: Enum[apply0] = this

    // Core
    //
    type succ = a => a
    def succ: succ = toEnum `.` ((_: Int) + 1) `.` fromEnum

    type pred = a => a
    def pred: pred = toEnum `.` ((_: Int) - 1) `.` fromEnum

    type toEnum = Int => a
    def toEnum: toEnum

    type fromEnum = a => Int
    def fromEnum: fromEnum

    type enumFrom = a => List[a]
    def enumFrom: enumFrom = x => {
        List.map(toEnum)(Int.enumFrom(fromEnum(x)))
    }

    type enumFromThen = a => a => List[a]
    def enumFromThen: enumFromThen = x => y => {
        List.map(toEnum)(Int.enumFromThen(fromEnum(x))(fromEnum(y)))
    }

    type enumFromTo = a => a => List[a]
    def enumFromTo: enumFromTo = x => y => {
        List.map(toEnum)(Int.enumFromTo(fromEnum(x))(fromEnum(y)))
    }

    type enumFromThenTo = a => a => a => List[a]
    def enumFromThenTo: enumFromThenTo = x1 => x2 => y => {
        List.map(toEnum)(Int.enumFromThenTo(fromEnum(x1))(fromEnum(x2))(fromEnum(y)))
    }
}


trait EnumProxy[a] extends Enum[a] {
    def selfEnum: Enum[a]

    override def succ: succ = selfEnum.succ
    override def pred: pred = selfEnum.pred
    override def toEnum: toEnum = selfEnum.toEnum
    override def fromEnum:fromEnum = selfEnum.fromEnum

    override def enumFrom: enumFrom = selfEnum.enumFrom
    override def enumFromThen: enumFromThen = selfEnum.enumFromThen
    override def enumFromTo: enumFromTo = selfEnum.enumFromTo
    override def enumFromThenTo: enumFromThenTo = selfEnum.enumFromThenTo
}


object Enum extends EnumInstance {
    def apply[a <: Kind.Function0](implicit i: Enum[a#apply0]): Enum[a#apply0] = i

    private[ken] def numericEnumFrom[a](n: a)(implicit i: Fractional[a]): List[a] = {
        import i._
        n :: numericEnumFrom(n + 1)
    }

    private[ken] def numericEnumFromThen[a](n: a)(m: a)(implicit i: Fractional[a]): List[a] = {
        import i._
        n :: numericEnumFromThen(m)(m+m-n)
    }

    private[ken] def numericEnumFromTo[a](n: a)(m: a)(implicit i: Ord[a], j: Fractional[a]): List[a] = {
        import i._
        import j._
        List.takeWhile((_: a) <= m + 1/2)(numericEnumFrom(n))
    }

    private[ken] def numericEnumFromThenTo[a](e1: a)(e2: a)(e3: a)(implicit i: Ord[a], j: Fractional[a]): List[a] = {
        import i._
        import j._
        val mid = (e2 - e1) / 2
        val predicate: a => Bool = {
            if (e2 >= e1) (_: a) <= e3 + mid
            else (_: a) >= e3 + mid
        }
        List.takeWhile(predicate)(numericEnumFromThen(e1)(e2))
    }
}


sealed trait EnumInstance { this: Enum.type =>
    implicit val ofBool: Enum[Bool] = _Bool
    implicit val ofChar: Enum[Char] = Char
    implicit val ofDouble: Enum[Double] = Double
    implicit val ofFloat: Enum[Float] = Float
    implicit val ofInt: Enum[Int] = Int
    implicit val ofInteger: Enum[Integer] = _Integer
    implicit val ofUnit: Enum[Unit] = Unit

    implicit def ofScalaNumeric[a](implicit i: scala.math.Numeric[a]): Enum[a] = new Enum[a] {
        override val toEnum: toEnum = n => i.fromInt(n)
        override val fromEnum: fromEnum = x => i.toInt(x)
    }
}
