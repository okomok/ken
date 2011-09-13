

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Fractional[a] extends Num[a] {
    final val asFractional: Fractional[apply0] = this

    // Core
    //
    type op_/ = a => a => a
    def op_/ : op_/ = x => y => x * recip(y)

    type recip = a => a
    def recip: recip = x => fromIntegral(1) / x

    type fromRational = Rational => a
    def fromRational: fromRational

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

    override def op_/ : op_/ = selfFractional.op_/
    override def recip: recip = selfFractional.recip
    override def fromRational: fromRational = selfFractional.fromRational

    override def realToFrac[z](x: z)(implicit i: Real[z]): a = selfFractional.realToFrac(x)(i)
}


object Fractional extends FractionalInstance with FractionalShortcut {
    def apply[a](implicit i: Fractional[a]): Fractional[a] = i
}


sealed trait FractionalInstance { this: Fractional.type =>
    implicit val ofDouble: Fractional[Double] = Double
    implicit val ofFloat: Fractional[Float] = Float
}


sealed trait FractionalShortcut { this: Fractional.type =>
    def op_/[a](x: a)(y: a)(implicit i: Fractional[a]): a = i.op_/(x)(y)
    def recip[a](x: a)(implicit i: Fractional[a]): a = i.recip(x)
    def fromRational[a](x: Rational)(implicit i: Fractional[a]): a = i.fromRational(x)

    implicit def realToFrac[z, a](x: z, * : Type[a] = null)(implicit i: Real[z], j: Fractional[a]): a = j.realToFrac(x)(i)

    sealed class _Op_/[a](x: a)(implicit i: Fractional[a]) {
        def /(y: a): a = op_/(x)(y)
    }
    implicit def /[a](x: a)(implicit i: Fractional[a]): _Op_/[a] = new _Op_/(x)
}
