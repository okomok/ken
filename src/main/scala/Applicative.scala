

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Applicative[f[_]] extends Functor[f] {
    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]

    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

    def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(apply))(x)(y)
    def liftA[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
    def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = x <#> y <*> z
    def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = x <#> y <*> z <*> w

    private[ken] class Op_<*>[a, b](x: f[a => b]) {
        def <*>(y: f[a]): f[b] = op_<*>(x)(y)
    }
    implicit def <*>[a, b](x: f[a => b]): Op_<*>[a, b] = new Op_<*>(x)

    private[ken] class Op_*>[a](x: f[a]) {
        def *>[b](y: f[b]): f[b] = op_*>(x)(y)
    }
    implicit def *>[a, b](x: f[a]): Op_*>[a] = new Op_*>(x)

    private[ken] class Op_<*[a](x: f[a]) {
        def <*[b](y: f[b]): f[a] = op_<*(x)(y)
    }
    implicit def <*[a](x: f[a]): Op_<*[a] = new Op_<*(x)

    private[ken] class Op_<**>[a](x: f[a]) {
        def <**>[b](y: f[a => b]): f[b] = op_<**>(x)(y)
    }
    implicit def <**>[a](x: f[a]): Op_<**>[a] = new Op_<**>(x)

    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
}
