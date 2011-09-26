

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


object Enum extends EnumInstance with EnumShortcut with EnumDetail {
    def apply[a <: Kind.Function0](implicit i: Enum[a#apply0]): Enum[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: Enum[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Enum[nt#apply0] = new Enum[nt#apply0] {
        override val succ: succ = a => j.newOf(i.succ(j.oldOf(a)))
        override val pred: pred = a => j.newOf(i.pred(j.oldOf(a)))
        override val toEnum: toEnum = n => j.newOf(i.toEnum(n))
        override val fromEnum:fromEnum = a => i.fromEnum(j.oldOf(a))

        override val enumFrom: enumFrom = x => for { ot <- i.enumFrom(j.oldOf(x)) } yield j.newOf(ot)
        override val enumFromThen: enumFromThen = x => y => for { ot <- i.enumFromThen(j.oldOf(x))(j.oldOf(y)) } yield j.newOf(ot)
        override val enumFromTo: enumFromTo = x => y => for { ot <- i.enumFromTo(j.oldOf(x))(j.oldOf(y)) } yield j.newOf(ot)
        override val enumFromThenTo: enumFromThenTo = x1 => x2 => y => for { ot <- i.enumFromThenTo(j.oldOf(x1))(j.oldOf(x2))(j.oldOf(y)) } yield j.newOf(ot)
    }

    def weak[nt <: Kind.Newtype0](implicit i: Enum[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Enum[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](i, j.coNewtype)
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

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Enum[ot], k: Kind.MethodList.Contains[ds, Enum]): Enum[nt] = deriving[Newtype0[nt, ot, _]]
}


sealed trait EnumShortcut { this: Enum.type =>
    def succ[a](a: a)(implicit i: Enum[a]): a = i.succ(a)
    def pred[a](a: a)(implicit i: Enum[a]): a = i.pred(a)
    def toEnum[a](n: Int)(implicit i: Enum[a]): a = i.toEnum(n)
    def fromEnum[a](a: a)(implicit i: Enum[a]): Int = i.fromEnum(a)

    def enumFrom[a](a: a)(implicit i: Enum[a]): List[a] = i.enumFrom(a)
    def enumFromThen[a](x: a)(y: a)(implicit i: Enum[a]): List[a] = i.enumFromThen(x)(y)
    def enumFromTo[a](x: a)(y: a)(implicit i: Enum[a]): List[a] = i.enumFromTo(x)(y)
    def enumFromThenTo[a](x1: a)(x2: a)(y: a)(implicit i: Enum[a]): List[a] = i.enumFromThenTo(x1)(x2)(y)
}


private[ken] sealed trait EnumDetail { this: Enum.type =>
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
