

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Fractional[a] extends Num[a] {
    final val asFractional: Fractional[apply0] = this

    // Core
    //
    def op_/ : a => a => a = x => y => x * recip(y)
    def recip: a => a = x => fromIntegral(1) / x
    def fromRational: Rational => a

    // Extra
    //
    // higher priority than Num.fromIntegral
    implicit def realToFrac[z](x: z)(implicit i: Real[z]): a = fromRational(i.toRational(x))

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

    override def realToFrac[z](x: z)(implicit i: Real[z]): a = selfFractional.realToFrac(x)(i)
}


object Fractional extends FractionalInstance {
    def apply[a](implicit i: Fractional[a]): Fractional[a] = i
}


sealed trait FractionalInstance { this: Fractional.type =>
    implicit val _ofDouble: Fractional[Double] = Double
    implicit val _ofFloat: Fractional[Float] = Float
}
