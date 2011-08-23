

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Float {
/*
    private val fromRat: Rational => Float = {
        case Ratio(n, 0) => {
            if (n > 0) {
                1/0
            } else if (n < 0) {
                -1/0
            } else {
                0/0
            }
        }
        case r@Ratio(n, d) => {
            if (n > 0) {
                fromRat_(r)
            } else if (n < 0) {
                -fromRat_(Ratio(-n, d))
            } else {
                encodeFloat(0)(0)
            }
        }
    }
*/
    private[ken] val _Float_asRealFrac: RealFrac[Float] = new RealFrac[Float] with NumProxy[Float] {
        private[this] type a = Float
        override val selfNum = Num[Kind.const[Float]]
        // Fractional
        override val op_/ : a => a => a = x => y => x / y
        override val recip: a => a = x => 1.0F / x
        override lazy val fromRational: Rational => a = error("todo")
        // Real
        override lazy val toRational: a => Rational = error("todo")
        // RealFrac
        override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = error("todo")
    }

    private[ken] val _Double_asRealFrac: RealFrac[Double] = new RealFrac[Double] with NumProxy[Double] {
        private[this] type a = Double
        override val selfNum = Num[Kind.const[Double]]
        // Fractional
        override val op_/ : a => a => a = x => y => x / y
        override val recip: a => a = x => 1.0D / x
        override lazy val fromRational: Rational => a = error("todo")
        // Real
        override lazy val toRational: a => Rational = error("todo")
        // RealFrac
        override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = error("todo")
    }
}
