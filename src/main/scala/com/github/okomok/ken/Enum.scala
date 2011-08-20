

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Enum[a] extends Typeclass0[a] { outer =>
    final val asEnum: Enum[apply0] = this

    // Core
    //
    def succ: a => a = toEnum compose ((_: Int) + 1) compose fromEnum
    def pred: a => a = toEnum compose ((_: Int) - 1) compose fromEnum
    def toEnum: Int => a
    def fromEnum: a => Int

    def enumFrom: a => List[a] = x => List.map(toEnum)(List.rangeFrom(fromEnum(x)))
    def enumFromThen: a => a => List[a] = x => y => {
        val (n, m) = (fromEnum(x), fromEnum(y))
        List.map(toEnum)(List.step(m - n)(List.rangeFrom(n)))
    }
    def enumFromTo: a => a => List[a] = x => y => List.map(toEnum)(List.range(fromEnum(x), fromEnum(y)+1))
    def enumFromThenTo: a => a => a => List[a] = x1 => x2 => y => {
        val (n, m) = (fromEnum(x1), fromEnum(x2))
        List.map(toEnum)(List.step(m - n)(List.range(n, fromEnum(y)+1)))
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
}


private[ken] trait EnumInstance { this: Enum.type =>
    implicit def _ofScalaNumeric[a](implicit i: scala.math.Numeric[a]): Enum[a] = new Enum[a] {
        override val toEnum: Int => a = n => i.fromInt(n)
        override val fromEnum: a => Int = x => i.toInt(x)
    }
}