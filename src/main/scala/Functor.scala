

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[+_]] extends Klass {
    type apply[+a] = f[a]

    final def asFunctor: Functor[f] = this

// Overridables
    def fmap[a, b](x: a => b)(y: f[a]): f[b]

// Utilities
    final def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
    final def op_<@[a, b](x: => a)(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

// Infix Operators
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
}


object Functor {
    def apply[f[+_]](implicit i: Functor[f]): Functor[f] = i

    implicit val ofWeakIdentity: Functor[({type m[+a] = a})#m] = WeakIdentity.monad
    implicit def ofFunction[r]: Functor[({type m[+a] = r => a})#m] = Function.monad[r]
}
