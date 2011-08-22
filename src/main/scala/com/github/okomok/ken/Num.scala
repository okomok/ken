

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Num[a] extends Typeclass0[a] {
    final val asNum: Num[apply0] = this

    // Core
    //
    def op_+ : a => a => a
    def op_- : a => a => a = { x => y => op_+(x)(negate(y)) }
    def op_* : a => a => a
    def negate: a => a = { x => op_-(fromInt(0))(x) }
    def abs: a => a
    def signum: a => a
    def fromInteger: Integer => a

    // Extra
    //
    def subtract: a => a => a = flip(op_-)
    def fromIntegral[z](x: z)(implicit i: Integral[z]): a = fromInteger(i.toInteger(x))

    // Operators
    //
    sealed class Op_+(x: a) {
        def +(y: a): a = op_+(x)(y)
    }
    final implicit def +(x: a): Op_+ = new Op_+(x)

    sealed class Op_-(x: a) {
        def -(y: a): a = op_-(x)(y)
    }
    final implicit def -(x: a): Op_- = new Op_-(x)

    sealed class Op_*(x: a) {
        def *(y: a): a = op_*(x)(y)
    }
    final implicit def *(x: a): Op_* = new Op_*(x)

    // Convenience
    //
    final implicit def fromInt(n: Int): a = fromInteger(n)
}


trait NumProxy[a] extends Num[a] {
    def selfNum: Num[a]

    override def op_+ : a => a => a = selfNum.op_+
    override def op_- : a => a => a = selfNum.op_-
    override def op_* : a => a => a = selfNum.op_*
    override def negate: a => a = selfNum.negate
    override def abs: a => a = selfNum.abs
    override def signum: a => a = selfNum.signum
    override def fromInteger: Integer => a = selfNum.fromInteger

    override def subtract: a => a => a = selfNum.subtract
    override def fromIntegral[z](x: z)(implicit i: Integral[z]): a = selfNum.fromIntegral(x)
}


object Num extends NumInstance {
    def apply[a <: Kind.Function0](implicit i: Num[a#apply0]): Num[a#apply0] = i
}


private[ken] trait NumInstance { this: Num.type =>
    implicit def _ofScalaNumeric[a](implicit i: scala.Numeric[a]): Num[a] = new Num[a] {
        override val op_+ : a => a => a = { x => y => i.plus(x, y) }
        override val op_- : a => a => a = { x => y => i.minus(x, y) }
        override val op_* : a => a => a = { x => y => i.times(x, y) }
        override val negate: a => a = { x => i.negate(x) }
        override val abs: a => a = { x => i.abs(x) }
        override val signum: a => a = { x => fromInteger(i.signum(x)) }
        override val fromInteger: Integer => a = { n => i.fromInt(n.toInt) }
    }

    implicit val _ofInteger: Num[Integer] = new Num[Integer] with NumProxy[Integer] {
        private[this] type a = Integer
        override val selfNum = _ofScalaNumeric[Integer]
        override val fromInteger: Integer => a = id
    }

    implicit def _Fractional_ofScalaFractional[a](implicit i: scala.math.Fractional[a]): Fractional[a] = new Fractional[a] with NumProxy[a] {
        override val selfNum = _ofScalaNumeric[a]
        override val op_/ : a => a => a = x => y => i.div(x, y)
        override lazy val fromRational: Rational => a = error("todo")
    }

    implicit val _Real_ofFloat: Real[Float] = new Real[Float] with NumProxy[Float] {
        private[this] type a = Float
        override val selfNum = _ofScalaNumeric[Float]
        override lazy val toRational: a => Rational = error("todo")
    }

    implicit val _Real_ofDouble: Real[Double] = new Real[Double] with NumProxy[Double] {
        private[this] type a = Double
        override val selfNum = _ofScalaNumeric[Double]
        override lazy val toRational: a => Rational = error("todo")
    }
}
