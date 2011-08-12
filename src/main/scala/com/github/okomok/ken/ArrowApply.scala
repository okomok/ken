

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowApply[a[-_, +_]] extends Arrow[a] {
    final val asArrowApply: ArrowApply[apply] = this

    // Core
    //
    def app[b, c]: a[(a[b, c], b), c]
}


trait ArrowApplyProxy[a[-_, +_]] extends ArrowApply[a] with ArrowProxy[a] {
    override def self: ArrowApply[a]

    override def app[b, c]: a[(a[b, c], b), c] = self.app[b, c]
}


object ArrowApply {
    def apply[a <: Kind.Function2](implicit i: ArrowApply[a#apply]): ArrowApply[a#apply] = i
}

