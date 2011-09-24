

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
/*
    // Blind
    //
    final case class Blind[a](override val get: a) extends Strong[a]

    object Blind extends Newtype1[Blind, ({type ot[+a] = a})#ot] with ThisIsInstance {
        // Overrides
        //
        // Newtype1
        private type nt[+a] = Blind[a]
        private type ot[+a] = a
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Blind(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.get

        // Instances
        //
        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Blind[a]] = new Arbitary[Blind[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Blind(x))(aa.arbitary)
            override val shrink: shrink = { case Blind(x) => for { x_ <- aa.shrink(x) } yield Blind(x_) }
        }
    }

    private[ken] sealed trait Blind_0 { this: Blind.type
        implicit def _asNum[a](implicit an: Num[a]): Num[Blind[a]] = Num.deriving[Blind.type, Kind.const[a]]
    }
*/
}
