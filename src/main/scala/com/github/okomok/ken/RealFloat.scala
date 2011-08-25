

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
    def floatRadix: a => Integer
    def floatDigits: a => Int
    def floatRange: a => (Int, Int)

    def decodeFloat: a => (Integer, Int)
    def encodeFloat: Integer => Int => a

    def exponent: a => Int = x => {
        val (m, n) = decodeFloat(x)
        if (m == 0) 0 else n + floatDigits(x)
    }

    def significand: a => a = x => {
        val (m, _) = decodeFloat(x)
        encodeFloat(m)(-floatDigits(x))
    }

    def scaleFloat: Int => a => a = k => x => {
        val (m, n) = decodeFloat(x)
        val (l, h) = floatRange(x)
        val d = floatDigits(x)
        val b = h - l + 4 * d
        encodeFloat(m)(n + clamp(b)(k))
    }

    def isNaN: a => Bool
    def isInfinite: a => Bool
    def isDenormalized: a => Bool
    def isNegativeZero: a => Bool
    def isIEEE: a => Bool

    def atan2: a => a => a = y => x => {
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

    override def floatRadix: a => Integer = selfRealFloat.floatRadix
    override def floatDigits: a => Int = selfRealFloat.floatDigits
    override def floatRange: a => (Int, Int) = selfRealFloat.floatRange

    override def decodeFloat: a => (Integer, Int) = selfRealFloat.decodeFloat
    override def encodeFloat: Integer => Int => a = selfRealFloat.encodeFloat

    override def exponent: a => Int = selfRealFloat.exponent
    override def significand: a => a = selfRealFloat.significand
    override def scaleFloat: Int => a => a = selfRealFloat.scaleFloat

    override def isNaN: a => Bool = selfRealFloat.isNaN
    override def isInfinite: a => Bool = selfRealFloat.isInfinite
    override def isDenormalized: a => Bool = selfRealFloat.isDenormalized
    override def isNegativeZero: a => Bool = selfRealFloat.isNegativeZero
    override def isIEEE: a => Bool = selfRealFloat.isIEEE

    override def atan2: a => a => a = selfRealFloat.atan2
}


object RealFloat {
    def apply[a <: Kind.Function0](implicit i: RealFloat[a#apply0]): RealFloat[a#apply0] = i
}


sealed trait RealFloatInstance { this: RealFloat.type =>
    implicit val _ofDouble: RealFloat[Double] = Double
    implicit val _ofFloat: RealFloat[Float] = Float
}
