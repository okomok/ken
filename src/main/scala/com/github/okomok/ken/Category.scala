

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// TODO


trait Category[cat[_, _]] extends Typeclass {
    type apply[a, b] = cat[a, b]

    def cid[a]: cat[a, a]
    def op_<<<[a, b, c](f: cat[b, c])(g: cat[a, b]): cat[a, c]

    final def op_>>>[a, b, c](f: cat[a, b])(g: cat[b, c]): cat[a, c] = op_<<<(g)(f)

    final private[ken] class Op_<<<[b, c](f: cat[b, c]) {
        def <<<[a](g: cat[a, b]): cat[a, c] = op_<<<(f)(g)
    }
    final implicit def <<<[b, c](f: cat[b, c]): Op_<<<[b, c] = new Op_<<<[b, c](f)

    final private[ken] class Op_>>>[a, b](f: cat[a, b]) {
        def >>>[c](g: cat[b, c]): cat[a, c] = op_>>>(f)(g)
    }
    final implicit def >>>[a, b](f: cat[a, b]): Op_>>>[a, b] = new Op_>>>[a, b](f)

}


object Category extends CategoryInstance {
    def apply[cat[_, _]](implicit i: Category[cat]): Category[cat] = i
}

trait CategoryInstance extends ArrowInstance
