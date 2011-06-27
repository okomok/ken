

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Category[cat[_, _]] {
    def id[a]: cat[a, a]
    def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c]
}


object Category extends CategoryOp with CategoryInstance {
    def id[cat[_, _], a](implicit i: Category[cat]): cat[a, a] = i.id[a]
}


trait CategoryOp {
    def op_<<<[cat[_, _], a, b, c](f: cat[b, c])(g: cat[a, b])(implicit i: Category[cat]): cat[a, c] = i.op_<<<(f)(g)
    def op_>>>[cat[_, _], a, b, c](f: cat[a, b])(g: cat[b, c])(implicit i: Category[cat]): cat[a, c] = i.op_<<<(g)(f)

    private[ken] class Op_<<<[cat[_, _], b, c](f: cat[b, c])(implicit i: Category[cat]) {
        def <<<[a](g: cat[a, b]): cat[a, c] = op_<<<(f)(g)
    }
    implicit def <<<[cat[_, _], b, c](f: cat[b, c])(implicit i: Category[cat]): Op_<<<[cat, b, c] = new Op_<<<[cat, b, c](f)

    private[ken] class Op_>>>[cat[_, _], a, b](f: cat[a, b])(implicit i: Category[cat]) {
        def >>>[c](g: cat[b, c]): cat[a, c] = op_>>>(f)(g)
    }
    implicit def >>>[cat[_, _], a, b](f: cat[a, b])(implicit i: Category[cat]): Op_>>>[cat, a, b] = new Op_>>>[cat, a, b](f)
}


trait CategoryInstance extends ApplicativeInstance
