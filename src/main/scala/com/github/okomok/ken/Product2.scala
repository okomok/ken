

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Product2 {
    def toTuple[x, y](p: Product2[x, y]): (x, y) = (p._1, p._2)
    def unapply[x, y](p: Product2[x, y]): Option[(x, y)] = Some(toTuple(p))
}
