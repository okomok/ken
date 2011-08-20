

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
    def negate: a => a = { x => op_-(fromInteger(0))(x) }
    def abs: a => a
    def signum: a => a
    def fromInteger: Integer => a

    // Extra
    //
    def subtract: a => a => a = flip(op_-)

    final implicit def fromInt(n: Int): a = fromInteger(n)

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
}


object Num extends NumInstance {
    def apply[a](implicit i: Num[a]): Num[a] = i
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
}
