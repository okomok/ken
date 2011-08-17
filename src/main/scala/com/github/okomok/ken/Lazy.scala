

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    def ! : a
}


object Lazy extends Monad[Lazy] with ThisIsInstance {
    implicit def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val ! : a = x
    }

    implicit def eval[a](x: Lazy[a]): a = x.!

    // Overrides
    //
    // Monad
    private[this] type m[+a] = Lazy[a]
    override def `return`[a](x: Lazy[a]): m[a] = Lazy { x }
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = Lazy { y(x.!).! }
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.!)
}
