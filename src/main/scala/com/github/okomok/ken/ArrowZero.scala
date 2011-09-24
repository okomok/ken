

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ArrowZero[a[-_, +_]] extends Arrow[a] {
    final val asArrowZero: ArrowZero[apply2] = this

    // Core
    //
    def zeroArrow[b, c]: a[b, c]
}


trait ArrowZeroProxy[a[-_, +_]] extends ArrowZero[a] with ArrowProxy[a] {
    def selfArrowZero: ArrowZero[a]
    override def selfArrow: Arrow[a] = selfArrowZero

    override def zeroArrow[b, c]: a[b, c] = selfArrowZero.zeroArrow[b, c]
}


object ArrowZero {
    def apply[a <: Kind.Function2](implicit i: ArrowZero[a#apply2]): ArrowZero[a#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit i: ArrowZero[nt#oldtype2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowZero[nt#apply2] = new ArrowZero[nt#apply2] with ArrowProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfArrow = Arrow.deriving[nt]

        override def zeroArrow[b, c]: a[b, c] = j.newOf(i.zeroArrow[b, c])
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowZero[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowZero[nt#oldtype2] = deriving[Kind.dualNewtype2[nt]](i, j.dual)
}
