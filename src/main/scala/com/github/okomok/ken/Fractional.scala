

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Fractional[a] extends Num[a] {
    final val asFractional: Fractional[apply0] = this

    // Core
    //
    def op_/ : a => a => a = x => y => x * recip(y)
    def recip: a => a = x => fromInt(1) / x
    def fromRational: Rational => a

    // Operators
    //
    sealed class Op_/(x: a) {
        def /(y: a): a = op_/(x)(y)
    }
    final implicit def /(x: a): Op_/ = new Op_/(x)
}


trait FractionalProxy[a] extends Fractional[a] {
    def selfFractional: Fractional[a]

    override def op_/ : a => a => a = selfFractional.op_/
    override def recip: a => a = selfFractional.recip
    override def fromRational: Rational => a = selfFractional.fromRational
}


object Fractional extends FractionalInstance {
    def apply[a](implicit i: Fractional[a]): Fractional[a] = i
}


private[ken] trait FractionalInstance { this: Fractional.type =>
    implicit def _ofScalaFractional[a](implicit i: scala.math.Fractional[a]): Fractional[a] = new Fractional[a] with NumProxy[a] {
        override val selfNum = Num._ofScalaNumeric[a]
        override val op_/ : a => a => a = x => y => i.div(x, y)
        override def fromRational: Rational => a = error("todo")
    }
}
