

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    def ! : a
}

object Lazy {
    def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val ! : a = x
    }

    // implicit def eval[a](x: Lazy[a]): a = x.!
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.!)
}


case class Lazy2[+a1, +a2](x1: Lazy[a1], x2: Lazy[a2])

object Lazy2 {
    def apply[a1, a2](x1: => a1, x2: => a2): Lazy2[a1, a2] = new Lazy2(Lazy(x1), Lazy(x2))
}
