

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowPlus[a[-_, +_]] extends ArrowZero[a] {
    final val asArrowPlus: ArrowPlus[apply2] = this

    // Core
    //
    def op_<+>[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c]

    // Operators
    //
    sealed class Op_<+>[b, c](f: a[b, c]) {
        def <+>(g: Lazy[a[b, c]]): a[b, c] = op_<+>(f)(g)
    }
    final implicit def <+>[b, c](f: a[b, c]): Op_<+>[b, c] = new Op_<+>[b, c](f)
}


trait ArrowPlusProxy[a[-_, +_]] extends ArrowPlus[a] with ArrowZeroProxy[a] {
    def selfArrowPlus: ArrowPlus[a]
    override def selfArrowZero: ArrowZero[a] = selfArrowPlus

    override def op_<+>[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c] = selfArrowPlus.op_<+>(f)(g)
}


object ArrowPlus {
    def apply[a <: Kind.Function2](implicit i: ArrowPlus[a#apply2]): ArrowPlus[a#apply2] = i

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: ArrowPlus[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): ArrowPlus[nt#apply2] = new ArrowPlus[nt#apply2] with ArrowZeroProxy[nt#apply2] {
        private[this] type a[-a, +b] = nt#apply2[a, b]
        override val selfArrowZero = ArrowZero.deriving[nt, ot](i, j)

        override def op_<+>[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c] = j.newOf(i.op_<+>(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowPlus[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowPlus[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}
