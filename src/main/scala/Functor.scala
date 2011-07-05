

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[_]] {
    def fmap[a, b](x: a => b)(y: f[a]): f[b]

    final def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
    final def op_<@[a, b](x: => a)(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

    final private[ken] class Op_<@>[a, b](x: a => b) {
        def <@>(y: f[a]): f[b] = op_<@>(x)(y)
    }
    final implicit def <@>[a, b](x: a => b): Op_<@>[a, b] = new Op_<@>[a, b](x)

    final private[ken] class Op_<@[a](x: => a) {
        def <@[b](y: f[b]): f[a] = op_<@(x)(y)
    }
    final implicit def <@[a](x: => a): Op_<@[a] = new Op_<@[a](x)
}

trait FunctorProxy[f[_]] extends Functor[f] with Proxy {
    override def self: Functor[f]
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = self.fmap(x)(y)
}


object Functor extends FunctorInstance {
    def apply[f[_]](implicit i: Functor[f]): Functor[f] = i
}

trait FunctorInstance extends ApplicativeInstance
