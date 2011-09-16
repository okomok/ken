

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


import scala.annotation.tailrec


trait RealFloat[a] extends RealFrac[a] with Floating[a] {
    final val asRealFloat: RealFloat[apply0] = this

    // Core
    //
    type floatRadix = Lazy[a] => Integer
    def floatRadix: floatRadix

    type floatDigits = Lazy[a] => Int
    def floatDigits: floatDigits

    type floatRange = Lazy[a] => (Int, Int)
    def floatRange: floatRange

    type decodeFloat = a => (Integer, Int)
    def decodeFloat: decodeFloat

    type encodeFloat = Integer => Int => a
    def encodeFloat: encodeFloat

    type exponent = a => Int
    def exponent: exponent = x => {
        val (m, n) = decodeFloat(x)
        if (m == 0) 0 else (n + floatDigits(x))
    }

    type significand = a => a
    def significand: significand = x => {
        val (m, _) = decodeFloat(x)
        encodeFloat(m)(-floatDigits(x))
    }

    type scaleFloat = Int => a => a
    def scaleFloat: scaleFloat = k => x => {
        val (m, n) = decodeFloat(x)
        val (l, h) = floatRange(x)
        val d = floatDigits(x)
        val b = h - l + 4 * d
        encodeFloat(m)(n + RealFloat.clamp(b)(k))
    }

    type isNaN = a => Bool
    def isNaN: isNaN

    type isInfinite = a => Bool
    def isInfinite: isInfinite

    type isDenormalized = a => Bool
    def isDenormalized: isDenormalized

    type isNegativeZero = a => Bool
    def isNegativeZero: isNegativeZero

    type isIEEE = a => Bool
    def isIEEE: isIEEE

    type atan2 = a => a => a
    def atan2: atan2 = y => x => {
        if (x > 0) {
            atan(y/x)
        } else if (x === 0 && y > 0) {
            pi/2
        } else if (x < 0 && y > 0) {
            pi + atan(y/x)
        } else if ((x <= 0 && y < 0) || (x < 0 && isNegativeZero(y)) || (isNegativeZero(x) && isNegativeZero(y))) {
            negate(atan2(negate(y))(x))
        } else if (y === 0 && (x < 0 || isNegativeZero(x))) {
            pi
        } else if (x === 0 && y === 0) {
            y
        } else {
            x + y
        }
    }
}


trait RealFloatProxy[a] extends RealFloat[a] with RealFracProxy[a] with FloatingProxy[a] {
    def selfRealFloat: RealFloat[a]
    override def selfRealFrac: RealFrac[a] = selfRealFloat
    override def selfFloating: Floating[a] = selfRealFloat

    override def floatRadix: floatRadix = selfRealFloat.floatRadix
    override def floatDigits: floatDigits = selfRealFloat.floatDigits
    override def floatRange: floatRange = selfRealFloat.floatRange

    override def decodeFloat: decodeFloat = selfRealFloat.decodeFloat
    override def encodeFloat: encodeFloat = selfRealFloat.encodeFloat

    override def exponent: exponent = selfRealFloat.exponent
    override def significand: significand = selfRealFloat.significand
    override def scaleFloat: scaleFloat = selfRealFloat.scaleFloat

    override def isNaN: isNaN = selfRealFloat.isNaN
    override def isInfinite: isInfinite = selfRealFloat.isInfinite
    override def isDenormalized: isDenormalized = selfRealFloat.isDenormalized
    override def isNegativeZero: isNegativeZero = selfRealFloat.isNegativeZero
    override def isIEEE: isIEEE = selfRealFloat.isIEEE

    override def atan2: atan2 = selfRealFloat.atan2
}


object RealFloat extends RealFloatInstance {
    def apply[a <: Kind.Function0](implicit i: RealFloat[a#apply0]): RealFloat[a#apply0] = i
/*
    private[ken] def fromRat[a](x: Rational)(implicit i: RealFloat[a]): a = {
        val Ratio(n, d) = x
        if (d == 0) {
            if (n > 0) 1/0
            else if (n < 0) -1/0
            else 0/0
        } else {
            if (n > 0) fromRat_(Ratio(n, d))
            else if (n < 0) -fromRat_(Ratio(-n, d))
            else i.encodeFloat(0)(0)
        }
    }
*/
    import java.lang.{Double => JDouble, Float => JFloat}

    private[ken] lazy val fromRatToDouble: Rational => Double = { case Ratio(n, d) =>
        if (d == 0) {
            if (n > 0) JDouble.POSITIVE_INFINITY
            else if (n < 0) JDouble.NEGATIVE_INFINITY
            else JDouble.NaN
        } else {
            if (n > 0) fromRat_(Ratio(n, d))(ofDouble)
            else if (n < 0) -fromRat_(Ratio(-n, d))(ofDouble)
            else 0.0D
        }
    }

    private[ken] lazy val fromRatToFloat: Rational => Float = { case Ratio(n, d) =>
        if (d == 0) {
            if (n > 0) JFloat.POSITIVE_INFINITY
            else if (n < 0) JFloat.NEGATIVE_INFINITY
            else JFloat.NaN
        } else {
            if (n > 0) fromRat_(Ratio(n, d))(ofFloat)
            else if (n < 0) -fromRat_(Ratio(-n, d))(ofFloat)
            else 0.0F
        }
    }

    private def fromRat_[a](x: Rational)(implicit i: RealFloat[a]): a = {
        val ir = RealFrac[Rational]
        import ir./
        import Int._max_
        lazy val b: Integer = i.floatRadix(r)
        lazy val p: Int = i.floatDigits(r)
        lazy val (minExp0: Int, _) = i.floatRange(r)
        lazy val minExp: Int = minExp0 - p
        lazy val xMin: Rational = Integer.toRational(expt(b)(p-1))
        lazy val xMax: Rational = Integer.toRational(expt(b)(p))
        lazy val p0: Int = (integerLogBase(b)(Ratio.numerator(x)) - integerLogBase(b)(Ratio.denominator(x)) - p) _max_ minExp
        lazy val f: Rational = if (p0 < 0) Ratio(1, expt(b)(-p0)) else Ratio(expt(b)(p0), 1)
        lazy val (x_, p_) = scaleRat(Integer.toRational(b))(minExp)(xMin)(xMax)(p0)(x/f)
        lazy val r: a = i.encodeFloat(ir.round[Integer](x_))(p_)
        r
    }

    @tailrec
    private def scaleRat(b: Rational)(minExp: Int)(xMin: Rational)(xMax: Rational)(p: Int)(x: Rational): (Rational, Int) = {
        val ir = RealFrac[Rational]
        import ir._
        if (p <= minExp) (x, p)
        else if (x >= xMax) scaleRat(b)(minExp)(xMin)(xMax)(p+1)(x/b)
        else if (x < xMin) scaleRat(b)(minExp)(xMin)(xMax)(p-1)(x*b)
        else (x, p)
    }

    private final val minExpt = 0
    private final val maxExpt = 1100

    private val expt: Integer => Int => Integer = base => n => {
        import Int._pow_
        if (base == 2 && n >= minExpt && n <= maxExpt) expts(minExpt + n)
        else base _pow_ n
    }

    private val expts: scala.Array[Integer] = {
        import Integer._pow_
        val xs = for { n <- Integer.enumFromTo(minExpt)(maxExpt) } yield (Int.toInteger(2) _pow_ n)
        xs.toScalaList.toArray
    }

    private val integerLogBase: Integer => Integer => Int = b => i => {
        if (i < b) 0
        else {
            val l = 2 * integerLogBase(b*b)(i)
            @tailrec
            def doDiv(x: Integer)(y: Int): Int = {
                import Integer._div_
                if (x < b) y
                else doDiv(x _div_ b)(y+1)
            }
            import Integer.{_div_, _pow_}
            doDiv(i _div_ (b _pow_ l))(l)
        }
    }

    private val clamp: Int => Int => Int = bd => k => Int.max(-bd)(Int.min(bd)(k))
}


sealed trait RealFloatInstance { this: RealFloat.type =>
    implicit val ofDouble: RealFloat[Double] = Double
    implicit val ofFloat: RealFloat[Float] = Float
}
