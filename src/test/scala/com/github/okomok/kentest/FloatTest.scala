

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._
import Float._


class FloatTest extends org.scalatest.junit.JUnit3Suite {

    def testDecode {
        expect((BigInt(12056327),10))(decodeFloat(12345678910F))

        expect((BigInt(0L),0))(decodeFloat(0.0F))
        expect((BigInt(0L),0))(decodeFloat(-0.0F))

        expect((BigInt(-13176406),-22))(decodeFloat(-3.1415F))

        // I don't know this is specified.
        expect((BigInt(8388608),105))(decodeFloat(java.lang.Float.POSITIVE_INFINITY))
    }

    def testEncode {
        expect(12345678910F)(encodeFloat(BigInt(12056327))(10))
        expect(0.0F)(encodeFloat(BigInt(0L))(0))
        expect(-0.3141513F)(encodeFloat(BigInt(-1317646))(-22))
    }

    def testRatinal {
        expect(Rational(1269257, 64))(toRational(19832.141F))
        expect(19832.141F)(fromRational(Rational(1269257, 64)))

        expect(Rational(-6588203, 2097152))(toRational(-3.1415F))
        expect(-3.1415F)(fromRational(Rational(-6588203, 2097152)))
    }

    def testTrigon {
        expect(1.1001867e-5F)(atan2(2.15431F)(195813.131F))
        expect(2.6655436F)(asinh(7.1531F))
        expect(2.6557708F)(acosh(7.1531F))
        expect(0.15431331F)(atanh(0.1531F))
    }

    def testMisc {
        expect(1.9720644e13F)(scaleFloat(31)(9183.141F))
        expect((BigInt(680L), 0.13128662F))(properFraction[Integer](680.1313F))
        expect(1845319)(round[Int](1845319.14131F))
    }

    def testEncode0 {
        expect(0.0D)(encodeFloat(BigInt(0L))(19))
        expect(0.0D)(encodeFloat(BigInt(0L))(-19))
    }
}
