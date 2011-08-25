

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import java.lang.{Math => JMath}
import java.lang.{Float => JFloat}


object Float extends Enum[Float] with Show[Float] with RealFloat[Float] {
    // Overrides
    //
    // Eq
    private type a = Float
    override val op_=== : a => a => Bool = x => y => x == y
    // Ord
    override val compare: a => a => Ordering = x => y => {
        if (x < y) LT
        else if (x == y) EQ
        else GT
    }
    override val op_< : a => a => Bool = x => y => x < y
    override val op_<= : a => a => Bool = x => y => x <= y
    override val op_> : a => a => Bool = x => y => x > y
    override val op_>= : a => a => Bool = x => y => x >= y
    // Num
    override val op_+ : a => a => a = x => y => x + y
    override val op_- : a => a => a = x => y => x - y
    override val op_* : a => a => a = x => y => x * y
    override val negate: a => a = x => -x
    override val abs: a => a = x => JMath.abs(x)
    override val signum: a => a = x => JMath.signum(x)
    override val fromInteger: Integer => a = x => x.toFloat
    // Enum
    override val succ: a => a = x => x + 1
    override val pred: a => a = x => x - 1
    override val toEnum: Int => a = n => n
    override val fromEnum: a => Int = x => x.toInt
    override val enumFrom: a => List[a] = Enum.numericEnumFrom
    override val enumFromThen: a => a => List[a] = Enum.numericEnumFromThen
    override val enumFromTo: a => a => List[a] = Enum.numericEnumFromTo
    override val enumFromThenTo: a => a => a => List[a] = Enum.numericEnumFromThenTo
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
    override val exp: a => a = x => JMath.exp(x.toDouble)
    override val log: a => a = x => JMath.log(x.toDouble)
    override val sqrt: a => a = x => JMath.sqrt(x.toDouble)
    override val op_** : a => a => a = x => y => JMath.pow(x.toDouble, y.toDouble)
    override lazy val logBase: a => a => a = error("todo")
    override val sin: a => a = x => JMath.sin(x.toDouble)
    override val cos: a => a = x => JMath.cos(x.toDouble)
    override val tan: a => a = x => JMath.tan(x.toDouble)
    override val asin: a => a = x => JMath.asin(x.toDouble)
    override val acos: a => a = x => JMath.acos(x.toDouble)
    override val atan: a => a = x => JMath.atan(x.toDouble)
    override val sinh: a => a = x => JMath.sinh(x.toDouble)
    override val cosh: a => a = x => JMath.cosh(x.toDouble)
    override val tanh: a => a = x => JMath.tanh(x.toDouble)
    override lazy val asinh: a => a = error("todo")
    override lazy val acosh: a => a = error("todo")
    override lazy val atanh: a => a = error("todo")
    // Show
    override lazy val showsPrec: Int => a => ShowS = error("todo") // showSignedFloat(showFloat)(x)
    override lazy val showList: List[a] => ShowS = showList__(showsPrec(0))
    // RealFloat
    override lazy val floatRadix: a => Integer = error("todo")
    override lazy val floatDigits: a => Int = error("todo")
    override lazy val floatRange: a => (Int, Int) = error("todo")
    override lazy val decodeFloat: a => (Integer, Int) = error("todo")
    override lazy val encodeFloat: Integer => Int => a = error("todo")
    override val exponent: a => Int = x => JMath.getExponent(x)
    override lazy val significand: a => a = error("todo")
    override lazy val scaleFloat: Int => a => a = error("todo")
    override val isNaN: a => Bool = x => JFloat.isNaN(x)
    override val isInfinite: a => Bool = x => JFloat.isInfinite(x)
    override lazy val isDenormalized: a => Bool = error("todo")
    override lazy val isNegativeZero: a => Bool = error("todo")
    override val isIEEE: a => Bool = const(True)
    override val atan2: a => a => a = x => y => JMath.atan2(x.toDouble, y.toDouble)

    private def showSignedFloat[a](showPos: a => ShowS)(p: Int)(x: a)(implicit i: RealFloat[a]): ShowS = {
        error("todo")
    }
/*
    private lazy val fromRat: Rational => Float = {
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
