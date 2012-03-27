

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
import scala.annotation.tailrec


object Float extends Enum[Float] with Eq.Of[Float] with RealFloat[Float] with Random[Float] with Show[Float] {
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
    override lazy val fromRational: fromRational = RealFloat.fromRatToFloat
    // Real
    override val toRational: toRational = x => {
        val (m, n) = decodeFloat(x)
        val b = floatRadix(x)
        val ir = Num[Rational]
        ir.op_*(Rational(m, 1))(Int.powpow(Rational(b, 1))(n))
    }
    // RealFrac
    private type a = Float
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
    override val decodeFloat: decodeFloat = _decodeFloat
    override val encodeFloat: encodeFloat = m => n => _encodeFloat(m, n)
    override val exponent: exponent = x => JMath.getExponent(x)
    override val isNaN: isNaN = x => JFloat.isNaN(x)
    override val isInfinite: isInfinite = x => JFloat.isInfinite(x)
    override val isDenormalized: isDenormalized = x => _expBits(x) == 0
    override val isNegativeZero: isNegativeZero = x => (x == 0.0D) && (_signBits(x) == 1)
    override val isIEEE: isIEEE = _ => True
    override val atan2: atan2 = x => y => JMath.atan2(x.toDouble, y.toDouble)
    // Random
    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = ival match {
        case (a, b) => Random.randomIvalDouble(Double.realToFrac(a), Double.realToFrac(b))(realToFrac[Double])(g)
    }
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = Random.randomIvalDouble(0D, 1D)(realToFrac[Double])(g)
    // Show
    override val showsPrec: showsPrec = x => showSignedFloat(showFloat)(x)
    override val showList: showList = Show.showList__(showsPrec(0))

    // Details
    //
    private final val _signMask = 0x80000000
    private final val _expMask = 0x7f800000
    private final val _fractMask = 0x007fffff
    private final val _expNormalizedBias = 127
    private final val _expDenormalizedBias = 126
    private final val _expBitCount = 8
    private final val _fractBitCount = 23

    private lazy val _signBits: Float => Int = x => ((JFloat.floatToRawIntBits(x) & _signMask) >>> (_expBitCount + _fractBitCount)).toInt
    private lazy val _expBits: Float => Int = x =>  ((JFloat.floatToRawIntBits(x) & _expMask) >>> _fractBitCount).toInt
    private lazy val _fractBits: Float => Int = x => JFloat.floatToRawIntBits(x) & _fractMask

    import WorkaroundNotFound._

    private lazy val _decodeFloat: Float => (Integer, Int) = x => {
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

    private lazy val _encodeFloat: Pair[Integer, Int] => Float = { case (m, n) =>
        val impl: Pair[Integer, Int] => Float = { case (m, n) =>
            if (m == 0) {
                0.0F
            } else {
                var bits: Int = 0
                if (m < 0) {
                    bits |= _signMask
                }
                bits |= (n + _expNormalizedBias + _fractBitCount) << _fractBitCount
                val fract = java.lang.Math.abs(m.toInt)
                bits |= (fract & _fractMask)
                JFloat.intBitsToFloat(bits)
            }
        }
        impl(_normalizedDecode(m, n))
    }

    // @scalacWorkaround("2.9.1") // scalac sucks - not found: value _isNormalizedDecode
    private object WorkaroundNotFound {
        lazy val _isNormalizedDecode: Pair[Integer, Int] => Bool = { case (m, n) =>
            import Integer._pow_
            val b: Integer = floatRadix(0.0F)
            val d: Int = floatDigits(0.0F)
            val m_ : Integer = Integer.abs(m)
            (m == 0 && n == 0) || ((b _pow_ (d-1)) <= m_ && m_ < (b _pow_ d))
        }

        final val _expNormalizedBias = 127

        @tailrec
        def _normalizedDecode(m_n: Pair[Integer, Int]): (Integer, Int) = m_n match { case (m, n) =>
            if (_isNormalizedDecode(m, n)) (m, n)
            else if (m == 0) (m, 0)
            else {
                import Integer._pow_
                val b: Integer = floatRadix(0.0F)
                val d: Int = floatDigits(0.0F)
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
}
