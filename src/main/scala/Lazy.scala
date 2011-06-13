

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

    // implicit def toStrict[a](x: &[a]): a = x.!
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.!)
}
