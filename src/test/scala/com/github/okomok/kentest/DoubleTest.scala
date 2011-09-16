

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class DoubleTest extends org.scalatest.junit.JUnit3Suite {

    def testDecode {
        expect((BigInt(6472691304366080L),-19))(Double.decodeFloat(12345678910D))

        expect((BigInt(0L),0))(Double.decodeFloat(0.0D))
        expect((BigInt(0L),0))(Double.decodeFloat(-0.0D))

        expect((BigInt(-7074029114692207L),-51))(Double.decodeFloat(-3.1415D))

        // I don't know this is specified.
        expect((BigInt(4503599627370496L),972))(Double.decodeFloat(java.lang.Double.POSITIVE_INFINITY))
    }

    def testEncode {
        expect(12345678910D)(Double.encodeFloat(BigInt(6472691304366080L))(-19))
        expect(0.0D)(Double.encodeFloat(BigInt(0L))(0))
        expect(-3.1415D)(Double.encodeFloat(BigInt(-7074029114692207L))(-51))
    }
}
