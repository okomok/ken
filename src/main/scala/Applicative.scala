

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._
import List.{Nil, ::, cons}


trait Applicative[f[_]] extends Functor[f] {
    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]

    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

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

    final def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(apply))(x)(y)
    final def liftA[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
    final def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = x <#> y <*> z
    final def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = x <#> y <*> z <*> w
}


object Applicative extends ApplicativeInstance

private[ken] trait ApplicativeInstance {
    implicit val ofId = Id

    implicit def ofFunction1[A]: Applicative[({type f[a] = A => a})#f] = new Applicative[({type f[a] = A => a})#f] {
        private[this] type f[a] = A => a
        override def pure[a](x: => a): f[a] = const(x)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = z => x(z)(y(z))
    }

    implicit def ofMonoid[a](implicit ma: Monoid[a]): Applicative[({type f[x] = (a, x)})#f] = new Applicative[({type f[x] = (a, x)})#f] {
        private[this] type f[x] = (a, x)
        override def pure[a](x: => a): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
