

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    private[ken] def eval: a
}

object Lazy {
    def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val eval: a = x
    }

    implicit def eval[a](x: Lazy[a]): a = x.eval
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.eval)
}
