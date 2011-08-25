

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


object RealFrac extends RealFracInstance {
    def apply[a <: Kind.Function0](implicit i: RealFrac[a#apply0]): RealFrac[a#apply0] = i
}


sealed trait RealFracInstance { this: RealFrac.type =>
    implicit val _ofFloat: RealFrac[Float] = Float
}
