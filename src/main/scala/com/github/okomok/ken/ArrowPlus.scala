

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ArrowPlus[a[-_, +_]] extends ArrowZero[a] {
    final val asArrowPlus: ArrowPlus[apply2] = this

    // Core
    //
    def op_<+>:[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c]

    // Operators
    //
    private[ken] sealed class Op_<+>:[b, c](g: Lazy[a[b, c]]) {
        def <+>:(f: a[b, c]): a[b, c] = op_<+>:(f)(g)
    }
    implicit def <+>:[b, c](g: => a[b, c]): Op_<+>:[b, c] = new Op_<+>:(Lazy(g))
}


trait ArrowPlusProxy[a[-_, +_]] extends ArrowPlus[a] with ArrowZeroProxy[a] {
    type selfArrowPlus = ArrowPlus[a]
    def selfArrowPlus: selfArrowPlus
    override def selfArrowZero: selfArrowZero = selfArrowPlus

    override def op_<+>:[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c] = selfArrowPlus.op_<+>:(f)(g)
}


object ArrowPlus {
    def apply[a <: Kind.Function2](implicit i: ArrowPlus[a#apply2]): ArrowPlus[a#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: ArrowPlus[nt#oldtype2]): ArrowPlus[nt#apply2] = new ArrowPlus[nt#apply2] with ArrowZeroProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfArrowZero: selfArrowZero = ArrowZero.deriving[nt]

        override def op_<+>:[b, c](f: a[b, c])(g: Lazy[a[b, c]]): a[b, c] = j.newOf(i.op_<+>:(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: ArrowPlus[nt#apply2]): ArrowPlus[nt#oldtype2] = deriving[Kind.coNewtype2[nt]](j.coNewtype, i)
}
