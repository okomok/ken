

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowZero[a[-_, +_]] extends Arrow[a] {
    final val asArrowZero: ArrowZero[apply] = this

    // Core
    //
    def zeroArrow[b, c]: a[b, c]
}


trait ArrowZeroProxy[a[-_, +_]] extends ArrowZero[a] with ArrowProxy[a] {
    override def self: ArrowZero[a]

    override def zeroArrow[b, c]: a[b, c] = self.zeroArrow[b, c]
}


object ArrowZero {
    def apply[a <: Kind.Function2](implicit i: ArrowZero[a#apply]): ArrowZero[a#apply] = i
}
