

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class Newtype0Test extends org.scalatest.junit.JUnit3Suite {

    final case class Wrap[a](override val get: a) extends NewtypeOf[a]

    object Wrap {
        implicit def _asNewType0[a]: Newtype0[Wrap[a], a, Num :^: Ord :^: Real :^: Enum :^: Integral :^: Floating :^: Kind.nil] = new Newtype0[Wrap[a], a, Num :^: Ord :^: Real :^: Enum :^: Integral :^: Floating :^: Kind.nil] {
            override val newOf: newOf = ot => Wrap(ot)
            override val oldOf: oldOf = nt => nt.get
        }
    }

    def testTrivial {
        Num[Wrap[Int]]
        Ord[Wrap[Int]]
        Integral[Wrap[Int]]
        Enum[Wrap[Int]]
        Real[Wrap[Int]]

        // Floating[Wrap[Int]] // must be error.
    }

}
