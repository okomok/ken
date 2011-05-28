

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Applicative extends Pointed {
    def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b]
    def op_*>[a, b](x: f_[a])(y: f_[b]): f_[b] = liftA2[a, b, b](const(identity))(x)(y)
    def op_<*[a, b](x: f_[a])(y: f_[b]): f_[a] = liftA2[a, b, a](const)(x)(y)

    override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = pure(x) <*> y

    def liftA2[a, b, c](x: a => b => c)(y: f_[a])(z: f_[b]): f_[c] = x <#> y <*> z

    class Op_<*>[a, b](x: f_[a => b]) {
        def <*>(y: f_[a]): f_[b] = op_<*>(x)(y)
    }
    implicit def <*>[a, b](x: f_[a => b]): Op_<*>[a, b] = new Op_<*>(x)

    class Op_*>[a](x: f_[a]) {
        def *>[b](y: f_[b]): f_[b] = op_*>(x)(y)
    }
    implicit def *>[a, b](x: f_[a]): Op_*>[a] = new Op_*>(x)

    class Op_<*[a](x: f_[a]) {
        def <*[b](y: f_[b]): f_[a] = op_<*(x)(y)
    }
    implicit def <*[a](x: f_[a]): Op_<*[a] = new Op_<*(x)
}
