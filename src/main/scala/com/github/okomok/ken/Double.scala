

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import java.lang.{Math => JMath}
import java.lang.{Double => JDouble}


object Double extends Enum[Double] with RealFloat[Double] with Show[Double] {
    // Overrides
    //
    // Eq
    private type a = Double
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
    override val fromInteger: Integer => a = x => x.toDouble
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
    override val recip: a => a = x => 1.0D / x
    override lazy val fromRational: Rational => a = error("todo")
    // Real
    override lazy val toRational: a => Rational = error("todo")
    // RealFrac
    override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = error("todo")
    // Floating
    override val pi: a = JMath.PI
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
    override val isNaN: a => Bool = JDouble.isNaN(_)
    override val isInfinite: a => Bool = JDouble.isInfinite(_)
    override lazy val isDenormalized: a => Bool = error("todo")
    override lazy val isNegativeZero: a => Bool = error("todo")
    override val isIEEE: a => Bool = const(True)
    override val atan2: a => a => a = x => y => JMath.atan2(x, y)
    // Show
    override lazy val showsPrec: Int => a => ShowS = error("todo") // showSignedFloat(showFloat)(x)
    override lazy val showList: List[a] => ShowS = showList__(showsPrec(0))
}
