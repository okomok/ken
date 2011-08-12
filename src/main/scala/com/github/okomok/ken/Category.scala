

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Category[cat[-_, +_]] extends Typeclass2[cat] {
    final val asCategory: Category[apply] = this

    // Core
    //
    def cid[a]: cat[a, a]
    def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c]

    // Extra
    //
    def op_>>>[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = op_<<<(g)(f)

    // Infix
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
    def apply[cat <: Kind.Function2](implicit i: Category[cat#apply]): Category[cat#apply] = i
}

private[ken] trait CategoryInstance { this: Category.type =>
    implicit val _ofFunction1: ArrowChoice[Function1] with ArrowApply[Function1] = Function._arrow
}
