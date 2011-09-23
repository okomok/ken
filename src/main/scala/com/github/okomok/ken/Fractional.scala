

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
    private[ken] sealed class Op_/(x: a) {
        def /(y: a): a = op_/(x)(y)
    }
    final implicit def /(x: a): Op_/ = new Op_/(x)
}


trait FractionalProxy[a] extends Fractional[a] with NumProxy[a] {
    def selfFractional: Fractional[a]
    override def selfNum = selfFractional

    override def op_/ : op_/ = selfFractional.op_/
    override def recip: recip = selfFractional.recip
    override def fromRational: fromRational = selfFractional.fromRational

    override def realToFrac[z](x: z)(implicit i: Real[z]): a = selfFractional.realToFrac(x)(i)
}


object Fractional extends FractionalInstance with FractionalShortcut {
    def apply[a](implicit i: Fractional[a]): Fractional[a] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Fractional[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Fractional[nt#apply0] = new Fractional[nt#apply0] with NumProxy[nt#apply0] {
        type a = nt#apply0
        override val selfNum = Num.deriving[nt, ot]

        override val op_/ : op_/ = x => y => j.newOf(i.op_/(j.oldOf(x))(j.oldOf(y)))
        override val recip: recip = x => j.newOf(i.recip(j.oldOf(x)))
        override val fromRational: fromRational = r => j.newOf(i.fromRational(r))

        override def realToFrac[z](x: z)(implicit zr: Real[z]): a = j.newOf(i.realToFrac(x)(zr))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Fractional[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Fractional[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
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
