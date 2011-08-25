

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
    def succ: a => a = toEnum compose ((_: Int) + 1) compose fromEnum
    def pred: a => a = toEnum compose ((_: Int) - 1) compose fromEnum
    def toEnum: Int => a
    def fromEnum: a => Int

    def enumFrom: a => List[a] = x => {
        List.map(toEnum)(Int.enumFrom(fromEnum(x)))
    }
    def enumFromThen: a => a => List[a] = x => y => {
        List.map(toEnum)(Int.enumFromThen(fromEnum(x))(fromEnum(y)))
    }
    def enumFromTo: a => a => List[a] = x => y => {
        List.map(toEnum)(Int.enumFromTo(fromEnum(x))(fromEnum(y)))
    }
    def enumFromThenTo: a => a => a => List[a] = x1 => x2 => y => {
        List.map(toEnum)(Int.enumFromThenTo(fromEnum(x1))(fromEnum(x2))(fromEnum(y)))
    }
}


trait EnumProxy[a] extends Enum[a] {
    def selfEnum: Enum[a]

    override def succ: a => a = selfEnum.succ
    override def pred: a => a = selfEnum.pred
    override def toEnum: Int => a = selfEnum.toEnum
    override def fromEnum: a => Int = selfEnum.fromEnum

    override def enumFrom: a => List[a] = selfEnum.enumFrom
    override def enumFromThen: a => a => List[a] = selfEnum.enumFromThen
    override def enumFromTo: a => a => List[a] = selfEnum.enumFromTo
    override def enumFromThenTo: a => a => a => List[a] = selfEnum.enumFromThenTo
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
    implicit val _ofBool: Enum[Bool] = _Bool
    implicit val _ofChar: Enum[Char] = Char
    implicit val _ofDouble: Enum[Double] = Double
    implicit val _ofFloat: Enum[Float] = Float
    implicit val _ofInt: Enum[Int] = Int
    implicit val _ofInteger: Enum[Integer] = _Integer
    implicit val _ofUnit: Enum[Unit] = Unit

    implicit def _ofScalaNumeric[a](implicit i: scala.math.Numeric[a]): Enum[a] = new Enum[a] {
        override val toEnum: Int => a = n => i.fromInt(n)
        override val fromEnum: a => Int = x => i.toInt(x)
    }
}
