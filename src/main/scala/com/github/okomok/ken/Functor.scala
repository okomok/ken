

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[+_]] extends Klass1[f] {
    final def asFunctor: Functor[f] = this

    // Core

    def fmap[a, b](x: a => b)(y: f[a]): f[b]

    // Extra

    def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
    def op_<@[a, b](x: => a)(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

    // Infix

    sealed class Infix_<@>[a, b](x: a => b) {
        def <@>(y: f[a]): f[b] = op_<@>(x)(y)
    }
    final implicit def <@>[a, b](x: a => b): Infix_<@>[a, b] = new Infix_<@>(x)

    sealed class Infix_<@[a](x: => a) {
        def <@[b](y: f[b]): f[a] = op_<@(x)(y)
    }
    final implicit def <@[a](x: a): Infix_<@[a] = new Infix_<@(x)
}


trait FunctorProxy[f[+_]] extends Functor[f] with Proxy {
    override def self: Functor[f]

    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = self.fmap(x)(y)
    override def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = self.op_<@>(x)(y)
    override def op_<@[a, b](x: => a)(y: f[b]): f[a] = self.op_<@(x)(y)
}


object Functor extends FunctorInstance {
    def apply[f[+_]](implicit i: Functor[f]): Functor[f] = i
}


trait FunctorInstance { this: Functor.type =>
    implicit val ofWeakIdentity: Functor[({type m[+a] = a})#m] = WeakIdentity.monad
    implicit def ofFunction[r]: Functor[({type m[+a] = r => a})#m] = Function.monad[r]
}
