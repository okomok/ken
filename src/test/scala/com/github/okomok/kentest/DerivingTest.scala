

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class DerivingTest extends org.scalatest.junit.JUnit3Suite {

    // `Deriving` supports only Eq, Show, and Ord.
    //
    final case class WrapSimple[a](override val _1: a, override val _2: a) extends Product2[a, a] with Deriving[Eq ^:: Show ^:: Ord ^:: Kind.Nil]

    def testDerivingShow {
        val ws = Show[WrapSimple[Int]]
        expect(false)(ws.isInstanceOf[Show.Default[_]])
        expect("WrapSimple(3,5)")(List.toJString(ws.show(WrapSimple(3, 5))))
    }

    def testDerivingShowList {
        val ws = Show[WrapSimple[String]]
        expect("WrapSimple(\"hello\",\"bye\")")(List.toJString(ws.show(WrapSimple[String]("hello", "bye"))))
    }

    def testDerivingEq {
        val ws = Eq[WrapSimple[Int]]
        expect(false)(ws.isInstanceOf[Eq.Default[_]])
        expect(true)(ws.op_===(WrapSimple(3, 4))(WrapSimple(3, 4)))
        expect(false)(ws.op_===(WrapSimple(3, 7))(WrapSimple(3, 5)))
    }

    def testDerivingOrd {
        val ws = Ord[WrapSimple[Int]]
        expect(true)(ws.op_<=(WrapSimple(3, 4))(WrapSimple(3, 4)))
        expect(true)(ws.op_<(WrapSimple(3, 4))(WrapSimple(3, 5)))
        expect(false)(ws.op_<(WrapSimple(3, 6))(WrapSimple(3, 5)))
    }
}
