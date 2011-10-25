

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


trait RealFrac[a] extends Real[a] with Fractional[a] {
    final val asRealFrac: RealFrac[apply0] = this

    // Core
    //
    def properFraction[b](x: a)(implicit i: Integral[b]): (b, a)

    def truncate[b](x: a)(implicit i: Integral[b]): b = {
        val (m, _) = properFraction(x)
        m
    }

    def round[b](x: a)(implicit i: Integral[b]): b = {
        val (n, r) = properFraction(x)
        val m = if (r < 0) {
            i.op_-(n)(i.fromIntegral(1))
        } else {
            i.op_+(n)(i.fromIntegral(1))
        }
        val s = signum(abs(r) - 0.5)
        if (s === -1) {
            n
        } else if (s === 0) {
            if (i.even(n)) n else m
        } else if (s === 1) {
            m
        } else {
            error("round default defn: Bad value")
        }
    }

    def ceiling[b](x: a)(implicit i: Integral[b]): b = {
        val (n, r) = properFraction(x)
        if (r > 0) {
            i.op_+(n)(i.fromIntegral(1))
        } else {
            n
        }
    }

    def floor[b](x: a)(implicit i: Integral[b]): b = {
        val (n, r) = properFraction(x)
        if (r < 0) {
            i.op_-(n)(i.fromIntegral(1))
        } else {
            n
        }
    }
}


trait RealFracProxy[a] extends RealFrac[a] with RealProxy[a] with FractionalProxy[a] {
    def selfRealFrac: RealFrac[a]
    override def selfReal: Real[a] = selfRealFrac
    override def selfFractional: Fractional[a] = selfRealFrac

    override def properFraction[b](x: a)(implicit i: Integral[b]): (b, a) = selfRealFrac.properFraction(x)(i)
    override def truncate[b](x: a)(implicit i: Integral[b]): b = selfRealFrac.truncate(x)(i)
    override def round[b](x: a)(implicit i: Integral[b]): b = selfRealFrac.round(x)(i)
    override def ceiling[b](x: a)(implicit i: Integral[b]): b = selfRealFrac.ceiling(x)(i)
    override def floor[b](x: a)(implicit i: Integral[b]): b = selfRealFrac.floor(x)(i)
}


object RealFrac extends RealFracInstance with RealFracShortcut {
    def apply[a <: Kind.Function0](implicit i: RealFrac[a#apply0]): RealFrac[a#apply0] = i

    def deriving[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: RealFrac[nt#oldtype]): RealFrac[nt#apply0] = new RealFrac[nt#apply0] with RealProxy[nt#apply0] with FractionalProxy[nt#apply0] {
        private type a = nt#apply0
        override val selfReal = Real.deriving[nt]
        override val selfFractional = Fractional.deriving[nt]

        override def properFraction[b](x: a)(implicit bi: Integral[b]): (b, a) = i.properFraction(j.oldOf(x))(bi) match { case (b, ot) => (b, j.newOf(ot)) }
        override def truncate[b](x: a)(implicit bi: Integral[b]): b = i.truncate(j.oldOf(x))(bi)
        override def round[b](x: a)(implicit bi: Integral[b]): b = i.round(j.oldOf(x))(bi)
        override def ceiling[b](x: a)(implicit bi: Integral[b]): b = i.ceiling(j.oldOf(x))(bi)
        override def floor[b](x: a)(implicit bi: Integral[b]): b = i.floor(j.oldOf(x))(bi)
    }

    def weak[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: RealFrac[nt#apply0]): RealFrac[nt#oldtype] = deriving[Kind.coNewtype[nt]](j.coNewtype, i)
}


sealed trait RealFracInstance { this: RealFrac.type =>
    implicit val _ofDouble: RealFrac[Double] = Double
    implicit val _ofFloat: RealFrac[Float] = Float

    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: RealFrac[ot], k: Kind.MethodList.Contains[ds, RealFrac]): RealFrac[nt] = deriving[Newtype[nt, ot, _]]
}


sealed trait RealFracShortcut { this: RealFrac.type =>
    def properFraction[a, b](x: a)(implicit i: RealFrac[a], j: Integral[b]): (b, a) = i.properFraction(x)(j)
    def truncate[a, b](x: a)(implicit i: RealFrac[a], j: Integral[b]): b = i.truncate(x)(j)
    def round[a, b](x: a)(implicit i: RealFrac[a], j: Integral[b]): b = i.round(x)(j)
    def ceiling[a, b](x: a)(implicit i: RealFrac[a], j: Integral[b]): b = i.ceiling(x)(j)
    def floor[a, b](x: a)(implicit i: RealFrac[a], j: Integral[b]): b = i.floor(x)(j)
}
