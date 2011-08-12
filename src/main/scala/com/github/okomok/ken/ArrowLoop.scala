

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowLoop[a[-_, +_]] extends Arrow[a] {
    final val asArrowLoop: ArrowLoop[apply2] = this

    // Core
    //
    def loop[b, c, d](f: a[(b, d), (c, d)]): a[b, c]
}


trait ArrowLoopProxy[a[-_, +_]] extends ArrowLoop[a] with ArrowProxy[a] {
    override def self: ArrowLoop[a]

    override def loop[b, c, d](f: a[(b, d), (c, d)]): a[b, c] = self.loop(f)
}


object ArrowLoop {
    def apply[a <: Kind.Function2](implicit i: ArrowLoop[a#apply2]): ArrowLoop[a#apply2] = i
}

