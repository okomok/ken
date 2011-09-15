

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


trait Alternative[f[+_]] extends Applicative[f] {
    final val asAlternative: Alternative[apply] = this

    // Core
    //
    def empty: f[Nothing]
    def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = List.op_!::[a]_ <@> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = List.op_!::[a]_ <@> v <*> many_v
        many_v
    }

    // Extra
    //
    def optional[a](x: f[a]): f[Maybe[a]] = (Just(_: a).up) <@> x <|> pure(Nothing.of[a])

    // Operators
    //
    private[ken] sealed class Op_<|>[a](x: f[a]) {
        def <|>(y: Lazy[f[a]]): f[a] = op_<|>(x)(y)
    }
    final implicit def <|>[a](x: f[a]): Op_<|>[a] = new Op_<|>(x)
}


trait AlternativeProxy[f[+_]] extends Alternative[f] with ApplicativeProxy[f] {
    def selfAlternative: Alternative[f]
    override def selfApplicative: Applicative[f] = selfAlternative

    override def empty: f[Nothing] = selfAlternative.empty
    override def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a] = selfAlternative.op_<|>(x)(y)
    override def some[a](v: f[a]): f[List[a]] = selfAlternative.some(v)
    override def many[a](v: f[a]): f[List[a]] = selfAlternative.many(v)

    override def optional[a](x: f[a]): f[Maybe[a]] = selfAlternative.optional(x)
}


object Alternative {
    def apply[f <: Kind.Function1](implicit i: Alternative[f#apply]): Alternative[f#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Alternative[ot#apply], j: Newtype1[nt#apply, ot#apply]): Alternative[nt#apply] = new Alternative[nt#apply] with ApplicativeProxy[nt#apply] {
        private type f[+a] = nt#apply[a]
        override val selfApplicative = Applicative.deriving[nt, ot](i, j)
        override def empty: f[Nothing] = j.newOf(i.empty)
        override def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a] = j.newOf(i.op_<|>(j.oldOf(x))(j.oldOf(y)))
        override def some[a](v: f[a]): f[List[a]] = j.newOf(i.some(j.oldOf(v)))
        override def many[a](v: f[a]): f[List[a]] = j.newOf(i.many(j.oldOf(v)))
        override def optional[a](x: f[a]): f[Maybe[a]] = j.newOf(i.optional(j.oldOf(x)))
    }

    def weak[nt <: Kind.Newtype1](implicit i: Alternative[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): Alternative[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
