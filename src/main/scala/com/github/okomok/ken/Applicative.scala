

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Applicative[f[+_]] extends Functor[f] {
    final val asApplicative: Applicative[apply] = this

    // Core
    //
    def pure[a](x: Lazy[a]): f[a]
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

    // Operators
    //
    private[ken] sealed class Op_<*>[a, b](x: f[a => b]) {
        def <*>(y: f[a]): f[b] = op_<*>(x)(y)
    }
    final implicit def <*>[a, b](x: f[a => b]): Op_<*>[a, b] = new Op_<*>(x)

    private[ken] sealed class Op_*>[a](x: f[a]) {
        def *>[b](y: f[b]): f[b] = op_*>(x)(y)
    }
    final implicit def *>[a](x: f[a]): Op_*>[a] = new Op_*>(x)

    private[ken] sealed class Op_<*[a](x: f[a]) {
        def <*[b](y: f[b]): f[a] = op_<*(x)(y)
    }
    final implicit def <*[a](x: f[a]): Op_<*[a] = new Op_<*(x)
}


trait ApplicativeProxy[f[+_]] extends Applicative[f] with FunctorProxy[f] {
    def selfApplicative: Applicative[f]
    override def selfFunctor: Functor[f] = selfApplicative

    override def pure[a](x: Lazy[a]): f[a] = selfApplicative.pure(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = selfApplicative.op_<*>(x)(y)
    override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = selfApplicative.op_*>(x)(y)
    override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = selfApplicative.op_<*(x)(y)

    override def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = selfApplicative.op_<**>(x)(y)
    override def liftA[a, b](x: a => b)(y: f[a]): f[b] = selfApplicative.liftA(x)(y)
    override def liftA2[a, b, c](x: a => b => c)(y: f[a])(z: f[b]): f[c] = selfApplicative.liftA2(x)(y)(z)
    override def liftA3[a, b, c, d](x: a => b => c => d)(y: f[a])(z: f[b])(w: f[c]): f[d] = selfApplicative.liftA3(x)(y)(z)(w)
}


object Applicative {
    def apply[f <: Kind.Function1](implicit i: Applicative[f#apply]): Applicative[f#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit i: Applicative[nt#oldtype1], j: Newtype1[nt#apply, nt#oldtype1]): Applicative[nt#apply] = new Applicative[nt#apply] with FunctorProxy[nt#apply] {
        private type f[+a] = nt#apply[a]
        override val selfFunctor = Functor.deriving[nt]

        override def pure[a](x: Lazy[a]): f[a] = j.newOf { i.pure(x) }
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = j.newOf { i.op_<*>(j.oldOf(x))(j.oldOf(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = j.newOf { i.op_*>(j.oldOf(x))(j.oldOf(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = j.newOf { i.op_<*(j.oldOf(x))(j.oldOf(y)) }

        // TODO
    }

    def weak[nt <: Kind.Newtype1](implicit i: Applicative[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): Applicative[nt#oldtype1] = deriving[Kind.dualNewtype1[nt]](i, j.dual)
}
