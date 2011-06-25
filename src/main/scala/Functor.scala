

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[_]] {
    def fmap[a, b](x: a => b)(y: f[a]): f[b]
}


object Functor extends FunctorOp with FunctorInstance


trait FunctorOp {
    def fmap[f[_], a, b](x: a => b)(y: f[a])(implicit i: Functor[f]): f[b] = i.fmap(x)(y)
}

trait FunctorInstance extends ApplicativeInstance {
}
