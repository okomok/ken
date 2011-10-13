

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class Newtype0Test extends org.scalatest.junit.JUnit3Suite {

    final case class Wrap[a](override val old: a) extends NewtypeOf[a]

    object Wrap {
        implicit def _asNewType0[a]: Newtype0[Wrap[a], a, Num ^:: Ord ^:: Real ^:: Enum ^:: Floating ^:: Show ^:: Eq ^:: Kind.Nil] = new Newtype0[Wrap[a], a, Num ^:: Ord ^:: Real ^:: Enum ^:: Floating ^:: Show ^:: Eq ^:: Kind.Nil] {
            override val newOf: newOf = ot => Wrap(ot)
            override val oldOf: oldOf = nt => nt.get
        }
    }

    def testTrivial {
        Num[Wrap[Int]]
        Ord[Wrap[Int]]
        Enum[Wrap[Int]]
        Real[Wrap[Int]]

        Integral[Kind.const[Int]] // but,...
        // Integral[Wrap[Int]] // must be error.

        // Floating[Wrap[Int]] // must be error.

    }

    def testShow {
        val ws = Show[Wrap[Int]]
        expect(false)(ws.isInstanceOf[Show.Default[_]])
        expect("Wrap(3)")(List.toJString(ws.show(Wrap(3))))

    }

    def testShowList {
        val ws = Show[Wrap[String]]
        expect(false)(ws.isInstanceOf[Show.Default[_]])
        expect("Wrap(\"hello\")")(List.toJString(ws.show(Wrap[String]("hello"))))
    }

    def testEq {
        val ws = Eq[Wrap[Int]]
        expect(false)(ws.isInstanceOf[Eq.Default[_]])
        expect(true)(ws.op_===(Wrap(3))(Wrap(3)))
        expect(false)(ws.op_===(Wrap(3))(Wrap(4)))
    }
}
