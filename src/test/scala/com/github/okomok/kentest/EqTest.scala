

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class EqTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val me = Eq[Kind.const[JString]]
        expect(true)(me.isInstanceOf[Eq.Default[_]])
    }

    final case class My(override val _1: Int, override val _2: String) extends Product2[Int, String]

    def testEqNoDeriving {
        val me = Eq[Kind.const[My]]
        expect(true)(me.isInstanceOf[Eq.Default[_]])
    }

    final case class Your(override val _1: Int, override val _2: String) extends Product2[Int, String] with Eq.Deriving

    def testEqDeriving {
        val me = Eq[Kind.const[Your]]
        expect(false)(me.isInstanceOf[Eq.Default[_]])
    }


}
