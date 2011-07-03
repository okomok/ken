

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Functor._


trait Functor[f[_]] {
    def fmap[a, b](x: a => b)(y: f[a]): f[b]
}

trait FunctorObj[f[+_], +a] extends Obj[f, a] {
    final def <@>[_a, b](y: f[_a])(implicit i: Functor[f], pre: f[a] <:< Function1[_a, b]): f[b] = op_<@>(pre(obj))(y)
//    final def <@[_a, b](y: f[_a])(implicit i: Functor[f], pre: f[a] <:< _a): f[b] = op_<@(pre(obj))(y)
}

trait FunctorProxy[f[_]] extends Functor[f] with Proxy {
    override def self: Functor[f]
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = self.fmap(x)(y)
}


object Functor extends FunctorOp with FunctorInstance


trait FunctorOp {
    def functor[f[_]](implicit i: Functor[f]): Functor[f] = i
    def fmap[f[_], a, b](x: a => b)(y: f[a])(implicit i: Functor[f]): f[b] = i.fmap(x)(y)

    def op_<@>[f[_], a, b](x: a => b)(y: f[a])(implicit i: Functor[f]): f[b] = i.fmap(x)(y)
    def op_<@[f[_], a, b](x: => a)(y: f[b])(implicit i: Functor[f]): f[a] = i.fmap[b, a](_ => x)(y)

    private[ken] class Op_<@>[f[_], a, b](x: a => b)(implicit i: Functor[f]) {
        def <@>(y: f[a]): f[b] = op_<@>(x)(y)
    }
    implicit def <@>[f[_], a, b](x: a => b)(implicit i: Functor[f]): Op_<@>[f, a, b] = new Op_<@>[f, a, b](x)

    private[ken] class Op_<@[f[_], a](x: => a)(implicit i: Functor[f]) {
        def <@[b](y: f[b]): f[a] = op_<@(x)(y)
    }
    implicit def <@[f[_], a](x: => a)(implicit i: Functor[f]): Op_<@[f, a] = new Op_<@[f, a](x)
}


trait FunctorInstance extends ApplicativeInstance
