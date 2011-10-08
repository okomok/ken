

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._
import Double._


class DoubleTest extends org.scalatest.junit.JUnit3Suite {

    def testDecode {
        expect((BigInt(6472691304366080L),-19))(decodeFloat(12345678910D))

        expect((BigInt(0L),0))(decodeFloat(0.0D))
        expect((BigInt(0L),0))(decodeFloat(-0.0D))

        expect((BigInt(-7074029114692207L),-51))(decodeFloat(-3.1415D))

        // I don't know this is specified.
        expect((BigInt(4503599627370496L),972))(decodeFloat(java.lang.Double.POSITIVE_INFINITY))
    }

    def testEncode {
        expect(12345678910D)(encodeFloat(BigInt(6472691304366080L))(-19))
        expect(0.0D)(encodeFloat(BigInt(0L))(0))
        expect(-3.1415D)(encodeFloat(BigInt(-7074029114692207L))(-51))
    }

    def testRatinal {
        expect(Rational(5451417408298287L, 274877906944L))(toRational(19832.141D))
        expect(19832.141D)(fromRational(Rational(5451417408298287L, 274877906944L)))

        expect(Rational(-7074029114692207L, 2251799813685248L))(toRational(-3.1415D))
        expect(-3.1415D)(fromRational(Rational(-7074029114692207L, 2251799813685248L)))
    }

    def testTrigon {
        expect(1.1001866876400031e-5)(atan2(2.15431D)(195813.131D))
        expect(2.665543561439427D)(asinh(7.1531D))
        expect(2.6557708297455114D)(acosh(7.1531D))
        expect(0.1543133114247586D)(atanh(0.1531D))
    }

    def testMisc {
        expect(2.814624526728544e58)(scaleFloat(181)(9183.141D))
        expect((BigInt(680L), 0.1313000000000102D))(properFraction[Integer](680.1313D))
        expect(1845319)(round[Int](1845319.14131D))
    }

    def testEncode0 {
        expect(0.0D)(encodeFloat(BigInt(0L))(19))
        expect(0.0D)(encodeFloat(BigInt(0L))(-19))
    }
}
