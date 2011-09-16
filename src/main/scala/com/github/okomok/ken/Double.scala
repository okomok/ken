

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
import java.lang.{Double => JDouble}


object Double extends Enum[Double] with Eq.Of[Double] with RealFloat[Double] with Show[Double] {
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
    override val fromInteger: fromInteger = x => x.toDouble
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
    override val recip: recip = x => 1.0D / x
    override lazy val fromRational: fromRational = RealFloat.fromRatToDouble
    // Real
    override val toRational: toRational = x => {
        val (m, n) = decodeFloat(x)
        val b = floatRadix(x)
        val ir = Num[Rational]
        Int.powpow(ir.op_*(Ratio(m, 1))(Ratio(b, 1)))(n)
    }
    // RealFrac
    private type a = Double
    override def properFraction[b](x: a)(implicit j : Integral[b]): (b, a) = decodeFloat(x) match {
        case (m, n) => {
            import Integer._pow_
            val b: Integer = floatRadix(x)
            if (n >= 0) {
                (j.op_*(j.fromInteger(m))(j.fromInteger(b) _pow_ n), 0.0D)
            } else {
                Integer.quotRem(m)(b _pow_ Int.negate(n)) match {
                    case (w, r) => (j.fromInteger(w), encodeFloat(r)(n))
                }
            }
        }
    }
    // Floating
    override val pi: pi = JMath.PI
    override val exp: exp = JMath.exp(_)
    override val log: log = JMath.log(_)
    override val sqrt: sqrt = JMath.sqrt(_)
    override val op_** : op_** = x => y => JMath.pow(x, y)
    override val logBase: logBase = x => y => log(y)/log(x)
    override val sin: sin = JMath.sin(_)
    override val cos: cos = JMath.cos(_)
    override val tan: tan = JMath.tan(_)
    override val asin: asin = JMath.asin(_)
    override val acos: acos = JMath.acos(_)
    override val atan: atan = JMath.atan(_)
    override val sinh: sinh = JMath.sinh(_)
    override val cosh: cosh = JMath.cosh(_)
    override val tanh: tanh = JMath.tanh(_)
    override val asinh: asinh = x => log(x + sqrt(1.0D+x*x))
    override val acosh: acosh = x => log(x + (x+1.0D) * sqrt((x-1.0D)/(x+1.0D)))
    override val atanh: atanh = x => 0.5D * log((1.0D+x)/(1.0D-x))
    // RealFloat
    override val floatRadix: floatRadix = _ => 2
    override val floatDigits: floatDigits = _ => 53
    override val floatRange: floatRange = _ => (-1021, 1024)
    override val decodeFloat: decodeFloat = _ => error("todo")
    override val encodeFloat: encodeFloat = _ => _ => error("todo")
    override val exponent: exponent = JMath.getExponent(_)
    override val significand: significand = _ => error("todo")
    override val scaleFloat: scaleFloat = _ => error("todo")
    override val isNaN: isNaN = JDouble.isNaN(_)
    override val isInfinite: isInfinite = JDouble.isInfinite(_)
    override val isDenormalized: isDenormalized = _ => error("todo")
    override val isNegativeZero: isNegativeZero = _ => error("todo")
    override val isIEEE: isIEEE = _ => True
    override val atan2: atan2 = x => y => JMath.atan2(x, y)
    // Show
    override val showsPrec: showsPrec = _ => _ => error("todo") // showSignedFloat(showFloat)(x)
}
