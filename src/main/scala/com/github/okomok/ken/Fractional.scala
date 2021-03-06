

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
    type selfFractional = Fractional[a]
    def selfFractional: selfFractional
    override def selfNum: selfNum = selfFractional

    override def op_/ : op_/ = selfFractional.op_/
    override def recip: recip = selfFractional.recip
    override def fromRational: fromRational = selfFractional.fromRational

    override def realToFrac[z](x: z)(implicit i: Real[z]): a = selfFractional.realToFrac(x)(i)
}


object Fractional extends FractionalInstance with FractionalShortcut {
    def apply[a](implicit i: Fractional[a]): Fractional[a] = i

    def deriving[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Fractional[nt#oldtype]): Fractional[nt#apply0] = new Fractional[nt#apply0] with NumProxy[nt#apply0] {
        private type a = nt#apply0
        override val selfNum: selfNum = Num.deriving[nt]

        override val op_/ : op_/ = x => y => j.newOf(i.op_/(j.oldOf(x))(j.oldOf(y)))
        override val recip: recip = x => j.newOf(i.recip(j.oldOf(x)))
        override val fromRational: fromRational = r => j.newOf(i.fromRational(r))

        override def realToFrac[z](x: z)(implicit zr: Real[z]): a = j.newOf(i.realToFrac(x)(zr))
    }

    def weak[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Fractional[nt#apply0]): Fractional[nt#oldtype] = deriving[Kind.coNewtype[nt]](j.coNewtype, i)
}


sealed trait FractionalInstance { this: Fractional.type =>
    implicit val _ofDouble: Fractional[Double] = Double
    implicit val _ofFloat: Fractional[Float] = Float

    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: Fractional[ot], k: Kind.MethodList.Contains[ds, Fractional]): Fractional[nt] = deriving[Newtype[nt, ot, _]]
}


trait FractionalShortcut extends NumShortcut {
    def op_/[a](x: a)(y: a)(implicit i: Fractional[a]): a = i.op_/(x)(y)
    def recip[a](x: a)(implicit i: Fractional[a]): a = i.recip(x)
    def fromRational[a](x: Rational)(implicit i: Fractional[a]): a = i.fromRational(x)

    implicit def realToFrac[z, a](x: z, * : Type[a] = null)(implicit i: Real[z], j: Fractional[a]): a = j.realToFrac(x)(i)

    private[ken] class _Op_/[a](x: a) {
        def /(y: a)(implicit i: Fractional[a]): a = op_/(x)(y)
    }
    implicit def /[a](x: a): _Op_/[a] = new _Op_/(x)
}
