

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Category[cat[-_, +_]] extends Typeclass2[cat] {
    final val asCategory: Category[apply2] = this

    // Core
    //
    def cid[a]: cat[a, a]
    def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c]

    // Extra
    //
    def op_>>>:[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = op_<<<:(g)(f)

    // Operators
    //
    private[ken] sealed class Op_<<<:[a, b](g: cat[a, b]) {
        def <<<:[c](f: cat[b, c]): cat[a, c] = op_<<<:(f)(g)
    }
    final implicit def <<<:[a, b](g: cat[a, b]): Op_<<<:[a, b] = new Op_<<<:(g)

    private[ken] sealed class Op_>>>:[b, c](g: cat[b, c]) {
        def >>>:[a](f: cat[a, b]): cat[a, c] = op_>>>:(f)(g)
    }
    final implicit def >>>:[b, c](g: cat[b, c]): Op_>>>:[b, c] = new Op_>>>:(g)
}


trait CategoryProxy[cat[-_, +_]] extends Category[cat] {
    def selfCategory: Category[cat]

    override def cid[a]: cat[a, a] = selfCategory.cid[a]
    override def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = selfCategory.op_<<<:(f)(g)

    override def op_>>>:[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = selfCategory.op_>>>:(f)(g)
}


object Category extends CategoryInstance {
    def apply[cat <: Kind.Function2](implicit i: Category[cat#apply2]): Category[cat#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: Category[nt#oldtype2]): Category[nt#apply2] = new Category[nt#apply2] {
        private type cat[-a, +b] = nt#apply2[a, b]

        override def cid[a]: cat[a, a] = j.newOf(i.cid[a])
        override def op_<<<:[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c] = j.newOf(i.op_<<<:(j.oldOf(f))(j.oldOf(g)))

        override def op_>>>:[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = j.newOf(i.op_>>>:(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: Category[nt#apply2]): Category[nt#oldtype2] = deriving[Kind.coNewtype2[nt]](j.coNewtype, i)
}


sealed trait CategoryInstance { this: Category.type =>
    implicit val _ofFunction: ArrowChoice[Function] with ArrowApply[Function] with ArrowLoop[Function] = Function
}
