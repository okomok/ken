

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


trait Applicative[f[+_]] extends Pointed[f] {
    final val asApplicative: Applicative[apply1] = this

    // Core
    //
    def op_<*>[a, b](x: f[a => b]): f[a] => f[b]
    def op_*>[a, b](x: f[a])(y: f[b]): f[b] = liftA2[a, b, b](const(id))(x)(y)
    def op_<*[a, b](x: f[a])(y: f[b]): f[a] = liftA2[a, b, a](const)(x)(y)

    // Overrides
    //
    // Functor
    override def fmap[a, b](x: a => b): f[a] => f[b] = y => pure(x) <*> y

    // Extra
    //
    def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = liftA2[a, a => b, b](flip(op_@))(x)(y)
    def liftA[a, b](x: a => b): f[a] => f[b] = y => pure(x) <*> y
    def liftA2[a, b, c](x: a => b => c): f[a] => f[b] => f[c] = y => z => x <@> y <*> z
    def liftA3[a, b, c, d](x: a => b => c => d): f[a] => f[b] => f[c] => f[d] = y => z => w => x <@> y <*> z <*> w

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


trait ApplicativeProxy[f[+_]] extends Applicative[f] with PointedProxy[f] {
    def selfApplicative: Applicative[f]
    override def selfPointed: Pointed[f] = selfApplicative

    override def op_<*>[a, b](x: f[a => b]): f[a] => f[b] = selfApplicative.op_<*>(x)
    override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = selfApplicative.op_*>(x)(y)
    override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = selfApplicative.op_<*(x)(y)

    override def op_<**>[a, b](x: f[a])(y: f[a => b]): f[b] = selfApplicative.op_<**>(x)(y)
    override def liftA[a, b](x: a => b): f[a] => f[b] = selfApplicative.liftA(x)
    override def liftA2[a, b, c](x: a => b => c): f[a] => f[b] => f[c] = selfApplicative.liftA2(x)
    override def liftA3[a, b, c, d](x: a => b => c => d): f[a] => f[b] => f[c] => f[d] = selfApplicative.liftA3(x)
}


object Applicative extends ApplicativeInstance {
    def apply[f <: Kind.Function1](implicit i: Applicative[f#apply1]): Applicative[f#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Applicative[nt#oldtype1]): Applicative[nt#apply1] = new Applicative[nt#apply1] with PointedProxy[nt#apply1] {
        private type f[+a] = nt#apply1[a]
        override val selfPointed = Pointed.deriving[nt]

        override def op_<*>[a, b](x: f[a => b]): f[a] => f[b] = y => j.newOf { i.op_<*>(j.oldOf(x))(j.oldOf(y)) }
        override def op_*>[a, b](x: f[a])(y: f[b]): f[b] = j.newOf { i.op_*>(j.oldOf(x))(j.oldOf(y)) }
        override def op_<*[a, b](x: f[a])(y: f[b]): f[a] = j.newOf { i.op_<*(j.oldOf(x))(j.oldOf(y)) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Applicative[nt#apply1]): Applicative[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait ApplicativeInstance { this: Applicative.type =>
}
