

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Category[cat[-_, +_]] extends Typeclass2[cat] {
    final val asCategory: Category[apply2] = this

    // Core
    //
    def cid[a]: cat[a, a]
    def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c]

    // Extra
    //
    def op_>>>[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = op_<<<(g)(f)

    // Operators
    //
    sealed class Op_<<<[b, c](f: cat[b, c]) {
        def <<<[a](g: cat[a, b]): cat[a, c] = op_<<<(f)(g)
    }
    final implicit def <<<[b, c](f: cat[b, c]): Op_<<<[b, c] = new Op_<<<[b, c](f)

    sealed class Op_>>>[a, b](f: cat[a, b]) {
        def >>>[c](g: cat[b, c]): cat[a, c] = op_>>>(f)(g)
    }
    final implicit def >>>[a, b](f: cat[a, b]): Op_>>>[a, b] = new Op_>>>[a, b](f)
}


trait CategoryProxy[cat[-_, +_]] extends Category[cat] with Proxy {
    override def self: Category[cat]

    override def cid[a]: cat[a, a] = self.cid[a]
    override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = self.op_<<<(f)(g)

    override def op_>>>[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = self.op_>>>(f)(g)
}


object Category extends CategoryInstance {
    def apply[cat <: Kind.Function2](implicit i: Category[cat#apply2]): Category[cat#apply2] = i

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: Category[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): Category[nt#apply2] = new Category[nt#apply2] {
        private[this] type cat[-a, +b] = nt#apply2[a, b]

        override def cid[a]: cat[a, a] = j.newOf(i.cid[a])
        override def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = j.newOf(i.op_<<<(j.oldOf(f))(j.oldOf(g)))

        override def op_>>>[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = j.newOf(i.op_>>>(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: Category[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): Category[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}


private[ken] trait CategoryInstance { this: Category.type =>
    implicit val _ofFunction1: ArrowChoice[Function1] with ArrowApply[Function1] with ArrowLoop[Function1] = Function
}
