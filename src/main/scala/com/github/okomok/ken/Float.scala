

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
        override val exp: a => a = JMath.exp(_)
        override val log: a => a = JMath.log(_)
        override val sqrt: a => a = JMath.sqrt(_)
        override val op_** : a => a => a = x => y => JMath.pow(x, y)
        override lazy val logBase: a => a => a = error("todo")
        override val sin: a => a = JMath.sin(_)
        override val cos: a => a = JMath.cos(_)
        override val tan: a => a = JMath.tan(_)
        override val asin: a => a = JMath.asin(_)
        override val acos: a => a = JMath.acos(_)
        override val atan: a => a = JMath.atan(_)
        override val sinh: a => a = JMath.sinh(_)
        override val cosh: a => a = JMath.cosh(_)
        override val tanh: a => a = JMath.tanh(_)
        override lazy val asinh: a => a = error("todo")
        override lazy val acosh: a => a = error("todo")
        override lazy val atanh: a => a = error("todo")
        // RealFloat
        override lazy val floatRadix: a => Integer = error("todo")
        override lazy val floatDigits: a => Int = error("todo")
        override lazy val floatRange: a => (Int, Int) = error("todo")
        override lazy val decodeFloat: a => (Integer, Int) = error("todo")
        override lazy val encodeFloat: Integer => Int => a = error("todo")
        override val exponent: a => Int = JMath.getExponent(_)
        override lazy val significand: a => a = error("todo")
        override lazy val scaleFloat: Int => a => a = error("todo")
        override val isNaN: a => Bool = JFloat.isNaN(_)
        override val isInfinite: a => Bool = JFloat.isInfinite(_)
        override lazy val isDenormalized: a => Bool = error("todo")
        override lazy val isNegativeZero: a => Bool = error("todo")
        override val isIEEE: a => Bool = const(True)
        override val atan2: a => a => a = x => y => JMath.atan2(x, y)
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
