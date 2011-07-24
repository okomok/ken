

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Applicative[f[+_]] extends Functor[f] {
// Overridables
    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]
    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

// Overrides
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y

// Utilities
    final def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(op_@))(x)(y)
    final def liftA[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
    final def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = x <@> y <*> z
    final def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = x <@> y <*> z <*> w

// Infix Operators
    sealed class Infix_<*>[a, b](x: f[a => b]) {
        def <*>(y: f[a]): f[b] = op_<*>(x)(y)
    }
    final implicit def <*>[a, b](x: f[a => b]): Infix_<*>[a, b] = new Infix_<*>(x)

    sealed class Infix_*>[a](x: f[a]) {
        def *>[b](y: f[b]): f[b] = op_*>(x)(y)
    }
    final implicit def *>[a](x: f[a]): Infix_*>[a] = new Infix_*>(x)

    sealed class Infix_<*[a](x: f[a]) {
        def <*[b](y: f[b]): f[a] = op_<*(x)(y)
    }
    final implicit def <*[a](x: f[a]): Infix_<*[a] = new Infix_<*(x)
}


trait ApplicativeProxy[f[+_]] extends Applicative[f] with FunctorProxy[f] {
    override def self: Applicative[f]
    override def pure[a](x: => a): f[a] = self.pure(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = self.op_<*>(x)(y)
    override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = self.op_*>(x)(y)
    override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = self.op_<*(x)(y)
}


object Applicative {
    def apply[f[+_]](implicit i: Applicative[f]): Applicative[f] = i

    implicit val ofWeakIdentity: Applicative[({type m[+a] = a})#m] = Identity.monad
    implicit def ofFunction[r]: Applicative[({type m[+a] = r => a})#m] = Function.monad[r]

    implicit def ofPair[z](implicit ma: Monoid[z]): Applicative[({type f[+a] = (z, a)})#f] = new Applicative[({type f[+a] = (z, a)})#f] {
        private[this] type f[a] = (z, a)
        override def pure[a](x: => a): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
