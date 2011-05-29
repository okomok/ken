

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Applicative extends Functor {
    def pure[a](x: => a): f_[a]
    def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b]

    def op_*>[a, b](x: f_[a])(y: f_[b]): f_[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f_[a])(y: f_[b]): f_[a] = liftA2[a, b, a](const)(x)(y)

    override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = pure(x) <*> y

    def op_<**>[a, b](x: f_[a])(y: f_[a => b]): f_[b] = liftA2[a, a => b, b](flip(apply))(x)(y)
    def liftA[a, b](x: a => b)(y: f_[a]): f_[b] = pure(x) <*> y
    def liftA2[a, b, c](x: a => b => c)(y: f_[a])(z: f_[b]): f_[c] = x <#> y <*> z
    def liftA3[a, b, c, d](x: a => b => c => d)(y: f_[a])(z: f_[b])(w: f_[c]): f_[d] = x <#> y <*> z <*> w

    private[ken] class _Op_<*>[a, b](x: f_[a => b]) {
        def <*>(y: f_[a]): f_[b] = op_<*>(x)(y)
    }
    implicit def <*>[a, b](x: f_[a => b]): _Op_<*>[a, b] = new _Op_<*>(x)

    private[ken] class _Op_*>[a](x: f_[a]) {
        def *>[b](y: f_[b]): f_[b] = op_*>(x)(y)
    }
    implicit def *>[a, b](x: f_[a]): _Op_*>[a] = new _Op_*>(x)

    private[ken] class _Op_<*[a](x: f_[a]) {
        def <*[b](y: f_[b]): f_[a] = op_<*(x)(y)
    }
    implicit def <*[a](x: f_[a]): _Op_<*[a] = new _Op_<*(x)

    private[ken] class _Op_<**>[a](x: f_[a]) {
        def <**>[b](y: f_[a => b]): f_[b] = op_<**>(x)(y)
    }
    implicit def <**>[a](x: f_[a]): _Op_<**>[a] = new _Op_<**>(x)
}
