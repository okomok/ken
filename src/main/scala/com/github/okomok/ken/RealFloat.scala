

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


trait RealFloat[a] extends RealFrac[a] with Floating[a] {
    final val asRealFloat: RealFloat[apply0] = this

    // Core
    //
    type floatRadix = a => Integer
    def floatRadix: floatRadix

    type floatDigits = a => Int
    def floatDigits: floatDigits

    type floatRange = a => (Int, Int)
    def floatRange: floatRange

    type decodeFloat = a => (Integer, Int)
    def decodeFloat: decodeFloat

    type encodeFloat = Integer => Int => a
    def encodeFloat: encodeFloat

    type exponent = a => Int
    def exponent: exponent = x => {
        val (m, n) = decodeFloat(x)
        if (m == 0) 0 else n + floatDigits(x)
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
        encodeFloat(m)(n + clamp(b)(k))
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

    private[this] def clamp(bd: Int)(k: Int): Int = {
        val i = Ord[Kind.const[Int]]
        i.max(-bd)(i.min(bd)(k))
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


object RealFloat {
    def apply[a <: Kind.Function0](implicit i: RealFloat[a#apply0]): RealFloat[a#apply0] = i
}


sealed trait RealFloatInstance { this: RealFloat.type =>
    implicit val ofDouble: RealFloat[Double] = Double
    implicit val ofFloat: RealFloat[Float] = Float
}
