

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
import scala.annotation.tailrec


object Double extends Enum[Double] with Eq.Default[Double] with RealFloat[Double] with Show.Default[Double] with Random[Double] {
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
        ir.op_*(Rational(m, 1))(Int.powpow(Rational(b, 1))(n))
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
    override val decodeFloat: decodeFloat = _decodeDouble
    override val encodeFloat: encodeFloat = m => n => _encodeDouble(m, n)
    override val exponent: exponent = x => JMath.getExponent(x)
    override val isNaN: isNaN = JDouble.isNaN(_)
    override val isInfinite: isInfinite = JDouble.isInfinite(_)
    override val isDenormalized: isDenormalized = x => _expBits(x) == 0
    override val isNegativeZero: isNegativeZero = x => (x == 0.0D) && (_signBits(x) == 1)
    override val isIEEE: isIEEE = _ => True
    override val atan2: atan2 = x => y => JMath.atan2(x, y)
    // Random
    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = Random.randomIvalDouble(ival)(id[Double])(g)
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = randomR(0D, 1D)(g)

    // Details
    //
    private final val _signMask = 0x8000000000000000L
    private final val _expMask = 0x7ff0000000000000L
    private final val _fractMask = 0x000fffffffffffffL
    private final val _expNormalizedBias = 1023
    private final val _expDenormalizedBias = 1022
    private final val _expBitCount = 11
    private final val _fractBitCount = 52

    private lazy val _signBits: Double => Int = x => ((JDouble.doubleToRawLongBits(x) & _signMask) >>> (_expBitCount + _fractBitCount)).toInt
    private lazy val _expBits: Double => Int = x =>  ((JDouble.doubleToRawLongBits(x) & _expMask) >>> _fractBitCount).toInt
    private lazy val _fractBits: Double => Long = x => JDouble.doubleToRawLongBits(x) & _fractMask

    private lazy val _decodeDouble: Double => (Integer, Int) = x => {
        val sign: Integer = if (_signBits(x) != 0) -1 else 1
        val exp: Int = _expBits(x)
        if (x == 0) {
            (0, 0)
        } else if (isDenormalized(x)) {
            _normalizedDecode(sign * _fractBits(x), exp - _expDenormalizedBias - _fractBitCount)
        } else {
            val fract = _fractBits(x) | (_fractMask + 1)
            (sign * fract, exp - _expNormalizedBias - _fractBitCount) ensuring _isNormalizedDecode
        }
    }

    private lazy val _encodeDouble: Pair[Integer, Int] => Double = { case (m, n) =>
        val impl: Pair[Integer, Int] => Double = { case (m, n) =>
            if (m == 0 && n == 0) {
                0.0D
            } else {
                var bits: Long = 0
                if (m < 0) {
                    bits |= _signMask
                }
                bits |= (n + _expNormalizedBias + _fractBitCount).toLong << _fractBitCount
                val fract = java.lang.Math.abs(m.toLong)
                bits |= (fract & _fractMask)
                JDouble.longBitsToDouble(bits)
            }
        }
        impl(_normalizedDecode(m, n))
    }

    private lazy val _isNormalizedDecode: Pair[Integer, Int] => Bool = { case (m, n) =>
        import Integer._pow_
        val b: Integer = floatRadix(0.0D)
        val d: Int = floatDigits(0.0D)
        val m_ : Integer = Integer.abs(m)
        (m == 0 && n == 0) || ((b _pow_ (d-1)) <= m_ && m_ < (b _pow_ d))
    }

    @tailrec
    private def _normalizedDecode(m_n: Pair[Integer, Int]): (Integer, Int) = m_n match { case (m, n) =>
        if (_isNormalizedDecode(m, n)) (m, n)
        else {
            import Integer._pow_
            val b: Integer = floatRadix(0.0D)
            val d: Int = floatDigits(0.0D)
            val m_ : Integer = Integer.abs(m)
            val sign: Integer = if (m < 0) -1 else 1
            if ( (b _pow_ (d-1)) > m_ ) {
                _normalizedDecode(sign * (m_ * b), n - 1)
            } else {
                _normalizedDecode(sign * (m_ / b), n + 1)
            }
        }
    }
}
