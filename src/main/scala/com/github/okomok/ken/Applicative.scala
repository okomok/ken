

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Applicative[f[+_]] extends Functor[f] {
    final val asApplicative: Applicative[apply] = this

    // Core
    //
    def pure[a](x: => a): f[a]
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b]
    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

    // Overrides
    //
    // Functor
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y

    // Extra
    //
    def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(op_@))(x)(y)
    def liftA[a, b](x: a => b)(y: f[a]): f[b] = pure(x) <*> y
    def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = x <@> y <*> z
    def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = x <@> y <*> z <*> w

    // Infix
    //
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

    override def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = self.op_<**>(x)(y)
    override def liftA[a, b](x: a => b)(y: f[a]): f[b] = self.liftA(x)(y)
    override def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = self.liftA2(x)(y)(z)
    override def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = self.liftA3(x)(y)(z)(w)
}


object Applicative {
    def apply[f <: Kind.Function1](implicit i: Applicative[f#apply]): Applicative[f#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Applicative[ot#apply], j: Newtype1[nt#apply, ot#apply]): Applicative[nt#apply] = new Applicative[nt#apply] with FunctorProxy[nt#apply] {
        private[this] type f[+a] = nt#apply[a]
        override val self = Functor.deriving[nt, ot](i, j)
        override def pure[a](x: => a): f[a] = j.new1 { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = j.new1 { i.op_<*>(j.old1(x))(j.old1(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = j.new1 { i.op_*>(j.old1(x))(j.old1(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = j.new1 { i.op_<*(j.old1(x))(j.old1(y)) }
    }

    def weak[nt <: Kind.Newtype1](implicit i: Applicative[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): Applicative[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
