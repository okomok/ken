

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


trait Num[a] extends Typeclass0[a] {
    final val asNum: Num[apply0] = this

    // Core
    //
    type op_+ = a => a => a
    def op_+ : op_+

    type op_- = a => a => a
    def op_- : op_- = x => y => op_+(x)(negate(y))

    type op_* = a => a => a
    def op_* : op_*

    type negate = a => a
    def negate: negate = x => op_-(fromIntegral(0))(x)

    type abs = a => a
    def abs: abs

    type signum = a => a
    def signum: signum

    type fromInteger = Integer => a
    def fromInteger: fromInteger

    // Extra
    //
    type subtract = a => a => a
    def subtract: subtract = flip(op_-)

    implicit def fromIntegral[z](x: z)(implicit i: Integral[z]): a = fromInteger(i.toInteger(x))

    // Operators
    //
    private[ken] sealed class Op_+(x: a) {
        def +(y: a): a = op_+(x)(y)
    }
    final implicit def +(x: a): Op_+ = new Op_+(x)

    private[ken] sealed class Op_-(x: a) {
        def -(y: a): a = op_-(x)(y)
    }
    final implicit def -(x: a): Op_- = new Op_-(x)

    private[ken] sealed class Op_*(x: a) {
        def *(y: a): a = op_*(x)(y)
    }
    final implicit def *(x: a): Op_* = new Op_*(x)

    private[ken] sealed class Op_unary_-(x: a) {
        def unary_- : a = negate(x)
    }
    final implicit def unary_-(x: a): Op_unary_- = new Op_unary_-(x)

    // Convenience
    //
    // final def fromInt(n: Int): a = fromInteger(n)
}


trait NumProxy[a] extends Num[a] {
    def selfNum: Num[a]

    override def op_+ : op_+ = selfNum.op_+
    override def op_- : op_- = selfNum.op_-
    override def op_* : op_* = selfNum.op_*
    override def negate: negate = selfNum.negate
    override def abs: abs = selfNum.abs
    override def signum: signum = selfNum.signum
    override def fromInteger: fromInteger = selfNum.fromInteger

    override def subtract: subtract = selfNum.subtract
    override def fromIntegral[z](x: z)(implicit i: Integral[z]): a = selfNum.fromIntegral(x)
}


object Num extends NumInstance with NumShortcut {
    def apply[a <: Kind.Function0](implicit i: Num[a#apply0]): Num[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Num[nt#oldtype0]): Num[nt#apply0] = new Num[nt#apply0] {
        private type a = nt#apply0

        override val op_+ : op_+ = x => y => j.newOf(i.op_+(j.oldOf(x))(j.oldOf(y)))
        override val op_- : op_- = x => y => j.newOf(i.op_-(j.oldOf(x))(j.oldOf(y)))
        override val op_* : op_* = x => y => j.newOf(i.op_*(j.oldOf(x))(j.oldOf(y)))
        override val negate: negate = x => j.newOf(i.negate(j.oldOf(x)))
        override val abs: abs = x => j.newOf(i.abs(j.oldOf(x)))
        override val signum: signum = x => j.newOf(i.signum(j.oldOf(x)))
        override val fromInteger: fromInteger = n => j.newOf(i.fromInteger(n))

        override val subtract: subtract = x => y => j.newOf(i.subtract(j.oldOf(x))(j.oldOf(y)))
        override def fromIntegral[z](x: z)(implicit zi: Integral[z]): a = j.newOf(i.fromIntegral(x)(zi))
    }

    def weak[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Num[nt#apply0]): Num[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](j.coNewtype, i)
}


sealed trait NumInstance { this: Num.type =>
    implicit val ofDouble: Num[Double] = Double
    implicit val ofFloat: Num[Float] = Float
    implicit val ofInt: Num[Int] = Int
    implicit val ofInteger: Num[Integer] = _Integer

    implicit def ofScalaNumeric[a](implicit i: scala.Numeric[a]): Num[a] = new Num[a] {
        override val op_+ : op_+ = x => y => i.plus(x, y)
        override val op_- : op_- = x => y => i.minus(x, y)
        override val op_* : op_* = x => y => i.times(x, y)
        override val negate: negate = x => i.negate(x)
        override val abs: abs = x => i.abs(x)
        override val signum: signum = x => fromInteger(i.signum(x))
        override val fromInteger: fromInteger = n => i.fromInt(n.toInt)
    }

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit j: Newtype0[nt, ot, ds], i: Num[ot], k: Kind.MethodList.Contains[ds, Num]): Num[nt] = deriving[Newtype0[nt, ot, _]]

/*
    implicit def _Fractional_ofScalaFractional[a](implicit i: scala.math.Fractional[a]): Fractional[a] = new Fractional[a] with NumProxy[a] {
        override val selfNum = ofScalaNumeric[a]
        override val op_/ : a => a => a = x => y => i.div(x, y)
        override lazy val fromRational: Rational => a = error("todo")
    }
*/
}


sealed trait NumShortcut { this: Num.type =>
    def op_+[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_+(x)(y)
    def op_-[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_-(x)(y)
    def op_*[a](x: a)(y: a)(implicit i: Num[a]): a = i.op_*(x)(y)
    def negate[a](x: a)(implicit i: Num[a]): a = i.negate(x)
    def abs[a](x: a)(implicit i: Num[a]): a = i.abs(x)
    def signum[a](x: a)(implicit i: Num[a]): a = i.signum(x)
    def fromInteger[a](n: Integer)(implicit i: Num[a]): a = i.fromInteger(n)

    def subtract[a](x: a)(y: a)(implicit i: Num[a]): a = i.subtract(x)(y)
    implicit def fromIntegral[z, a](x: z, * : Type[a] = null)(implicit i: Integral[z], j: Num[a]): a = j.fromIntegral(x)

    private[ken] class _Op_+[a](x: a)(implicit i: Num[a]) {
        def +(y: a): a = op_+(x)(y)
    }
    implicit def +[a](x: a)(implicit i: Num[a]): _Op_+[a] = new _Op_+(x)

    private[ken] class _Op_-[a](x: a)(implicit i: Num[a]) {
        def -(y: a): a = op_-(x)(y)
    }
    implicit def -[a](x: a)(implicit i: Num[a]): _Op_-[a] = new _Op_-(x)

    private[ken] class _Op_*[a](x: a)(implicit i: Num[a]) {
        def *(y: a): a = op_*(x)(y)
    }
    implicit def *[a](x: a)(implicit i: Num[a]): _Op_*[a] = new _Op_*(x)

    private[ken] class _Op_unary_-[a](x: a)(implicit i: Num[a]) {
        def unary_- : a = negate(x)
    }
    implicit def unary_-[a](x: a)(implicit i: Num[a]): _Op_unary_-[a] = new _Op_unary_-(x)
}
