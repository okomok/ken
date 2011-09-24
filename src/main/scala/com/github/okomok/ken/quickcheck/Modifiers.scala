

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


object Modifiers {

    // Blind
    //
    final case class Blind[a](override val get: a) extends NewtypeOf[a]

    object Blind {
        // Instances
        //
        implicit def _asNewType0[a]: Newtype0[Blind[a], a] = new Newtype0[Blind[a], a] {
            override val newOf: newOf = ot => Blind(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Blind[a]] = new Arbitary[Blind[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Blind(x))(aa.arbitary)
            override val shrink: shrink = { case Blind(x) => for { x_ <- aa.shrink(x) } yield Blind(x_) }
        }
    }

    private[ken] sealed trait Blind_0 { this: Blind.type =>
        implicit def _asNum[a](implicit an: Num[a]): Num[Blind[a]] = Num.deriving[Blind[a]]
    }

}
