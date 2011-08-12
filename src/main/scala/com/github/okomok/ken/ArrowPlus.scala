

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowPlus[a[-_, +_]] extends ArrowZero[a] {
    final val asArrowPlus: ArrowPlus[apply2] = this

    // Core
    //
    def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c]

    // Infix
    //
    sealed class Op_<+>[b, c](f: a[b, c]) {
        def <+>(g: => a[b, c]): a[b, c] = op_<+>(f)(g)
    }
    final implicit def <+>[b, c](f: a[b, c]): Op_<+>[b, c] = new Op_<+>[b, c](f)
}


trait ArrowPlusProxy[a[-_, +_]] extends ArrowPlus[a] with ArrowZeroProxy[a] {
    override def self: ArrowPlus[a]

    override def op_<+>[b, c](f: a[b, c])(g: => a[b, c]): a[b, c] = self.op_<+>(f)(g)
}


object ArrowPlus {
    def apply[a <: Kind.Function2](implicit i: ArrowPlus[a#apply2]): ArrowPlus[a#apply2] = i
}
