

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import java.lang.{Math => JMath}
import java.lang.{Float => JFloat}


object Float {
    private[ken] val _asRealFloat: RealFloat[Float] = new RealFloat[Float] with NumProxy[Float] {
        private[this] type a = Float
        override val selfNum = Num._ofScalaNumeric[Float]
        // Fractional
        override val op_/ : a => a => a = x => y => x / y
        override val recip: a => a = x => 1.0F / x
        override lazy val fromRational: Rational => a = error("todo")
        // Real
        override lazy val toRational: a => Rational = error("todo")
        // RealFrac
        override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = error("todo")
        // Floating
        override val pi: a = JMath.PI.toFloat // suppress realToFrac by toFloat.
        override val exp: a => a = x => JMath.exp(x.toFloat)
        override val log: a => a = x => JMath.log(x.toFloat)
        override val sqrt: a => a = x => JMath.sqrt(x.toFloat)
        override val op_** : a => a => a = x => y => JMath.pow(x.toFloat, y.toFloat)
        override lazy val logBase: a => a => a = error("todo")
        override val sin: a => a = x => JMath.sin(x.toFloat)
        override val cos: a => a = x => JMath.cos(x.toFloat)
        override val tan: a => a = x => JMath.tan(x.toFloat)
        override val asin: a => a = x => JMath.asin(x.toFloat)
        override val acos: a => a = x => JMath.acos(x.toFloat)
        override val atan: a => a = x => JMath.atan(x.toFloat)
        override val sinh: a => a = x => JMath.sinh(x.toFloat)
        override val cosh: a => a = x => JMath.cosh(x.toFloat)
        override val tanh: a => a = x => JMath.tanh(x.toFloat)
        override lazy val asinh: a => a = error("todo")
        override lazy val acosh: a => a = error("todo")
        override lazy val atanh: a => a = error("todo")
        // RealFloat
        override lazy val floatRadix: a => Integer = error("todo")
        override lazy val floatDigits: a => Int = error("todo")
        override lazy val floatRange: a => (Int, Int) = error("todo")
        override lazy val decodeFloat: a => (Integer, Int) = error("todo")
        override lazy val encodeFloat: Integer => Int => a = error("todo")
        override val exponent: a => Int = x => JMath.getExponent(x.toFloat)
        override lazy val significand: a => a = error("todo")
        override lazy val scaleFloat: Int => a => a = error("todo")
        override val isNaN: a => Bool = x => JFloat.isNaN(x.toFloat)
        override val isInfinite: a => Bool = x => JFloat.isInfinite(x.toFloat)
        override lazy val isDenormalized: a => Bool = error("todo")
        override lazy val isNegativeZero: a => Bool = error("todo")
        override val isIEEE: a => Bool = const(True)
        override val atan2: a => a => a = x => y => JMath.atan2(x.toFloat, y.toFloat)
    }
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
}
