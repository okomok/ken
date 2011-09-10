

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


import java.lang.{Math => JMath}
import java.lang.{Float => JFloat}


object Float extends Enum[Float] with Eq.Of[Float] with RealFloat[Float] with Show[Float] {
    // Overrides
    //
    // Ord
    override val compare: compare = x => y => {
        if (x < y) LT
        else if (x == y) EQ
        else GT
    }
    override val op_< : op_< = x => y => x < y
    override val op_<= : op_<= = x => y => x <= y
    override val op_> : op_> = x => y => x > y
    override val op_>= : op_>= = x => y => x >= y
    // Num
    override val op_+ : op_+ = x => y => x + y
    override val op_- : op_- = x => y => x - y
    override val op_* : op_* = x => y => x * y
    override val negate: negate = x => -x
    override val abs: abs = x => JMath.abs(x)
    override val signum: signum = x => JMath.signum(x)
    override val fromInteger: fromInteger = x => x.toFloat
    // Enum
    override val succ: succ = x => x + 1
    override val pred: pred = x => x - 1
    override val toEnum: toEnum = n => n
    override val fromEnum: fromEnum = x => x.toInt
    override val enumFrom: enumFrom = Enum.numericEnumFrom
    override val enumFromThen: enumFromThen = Enum.numericEnumFromThen
    override val enumFromTo: enumFromTo = Enum.numericEnumFromTo
    override val enumFromThenTo: enumFromThenTo = Enum.numericEnumFromThenTo
    // Fractional
    override val op_/ : op_/ = x => y => x / y
    override val recip: recip = x => 1.0F / x
    override val fromRational: fromRational = _ => error("todo")
    // Real
    override val toRational: toRational = _ => error("todo")
    // RealFrac
    private type a = Float
    override def properFraction[b](r: a)(implicit j : Integral[b]): (b, a) = error("todo")
    // Floating
    override val pi: pi = JMath.PI.toFloat // suppress realToFrac by toFloat.
    override val exp: exp = x => JMath.exp(x.toDouble)
    override val log: log = x => JMath.log(x.toDouble)
    override val sqrt: sqrt = x => JMath.sqrt(x.toDouble)
    override val op_** : op_** = x => y => JMath.pow(x.toDouble, y.toDouble)
    override val logBase: logBase = x => y => log(y)/log(x)
    override val sin: sin = x => JMath.sin(x.toDouble)
    override val cos: cos = x => JMath.cos(x.toDouble)
    override val tan: tan = x => JMath.tan(x.toDouble)
    override val asin: asin = x => JMath.asin(x.toDouble)
    override val acos: acos = x => JMath.acos(x.toDouble)
    override val atan: atan = x => JMath.atan(x.toDouble)
    override val sinh: sinh = x => JMath.sinh(x.toDouble)
    override val cosh: cosh = x => JMath.cosh(x.toDouble)
    override val tanh: tanh = x => JMath.tanh(x.toDouble)
    override val asinh: asinh = x => log(x + sqrt(1.0F+x*x))
    override val acosh: acosh = x => log(x + (x+1.0F) * sqrt((x-1.0F)/(x+1.0F)))
    override val atanh: atanh = x => 0.5F * log((1.0F+x)/(1.0F-x))
    // RealFloat
    override val floatRadix: floatRadix = _ => 2
    override val floatDigits: floatDigits = _ => 24
    override val floatRange: floatRange = _ => (-125, 128)
    override val decodeFloat: decodeFloat = _ => error("todo")
    override val encodeFloat: encodeFloat = _ => _ => error("todo")
    override val exponent: exponent = x => JMath.getExponent(x)
    override val significand: significand = _ => error("todo")
    override val scaleFloat: scaleFloat = _ => _ => error("todo")
    override val isNaN: isNaN = x => JFloat.isNaN(x)
    override val isInfinite: isInfinite = x => JFloat.isInfinite(x)
    override val isDenormalized: isDenormalized = _ => error("todo")
    override val isNegativeZero: isNegativeZero = _ => error("todo")
    override val isIEEE: isIEEE = _ => True
    override val atan2: atan2 = x => y => JMath.atan2(x.toDouble, y.toDouble)
    // Show
    override val showsPrec: showsPrec = _ => _ => error("todo") // showSignedFloat(showFloat)(x)

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
