

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    def ! : a
}

object Lazy extends Monad[Lazy] {
    def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val ! : a = x
    }

// Overrides
    private[this] type m[+a] = Lazy[a]
    // Monad
    override def `return`[a](x: a): m[a] = Lazy { x }
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = Lazy { y(x.!).! }

// Instances
    implicit val monad: Monad[Lazy] = this

    // implicit def eval[a](x: Lazy[a]): a = x.!
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.!)
}


case class Lazy2[+a1, +a2](x1: Lazy[a1], x2: Lazy[a2])

object Lazy2 {
    def apply[a1, a2](x1: => a1, x2: => a2): Lazy2[a1, a2] = new Lazy2(Lazy(x1), Lazy(x2))
}
