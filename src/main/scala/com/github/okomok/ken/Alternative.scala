

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
    final val asAlternative: Alternative[apply1] = this

    // Core
    //
    type empty = f[Nothing]
    def empty: empty

    def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = Function.from(List.op_!::[a]) <@> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = Function.from(List.op_!::[a]) <@> v <*> many_v
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
    type selfAlternative = Alternative[f]
    def selfAlternative: selfAlternative
    override def selfApplicative: selfApplicative = selfAlternative

    override def empty: empty = selfAlternative.empty
    override def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a] = selfAlternative.op_<|>(x)(y)
    override def some[a](v: f[a]): f[List[a]] = selfAlternative.some(v)
    override def many[a](v: f[a]): f[List[a]] = selfAlternative.many(v)

    override def optional[a](x: f[a]): f[Maybe[a]] = selfAlternative.optional(x)
}


object Alternative {
    def apply[f <: Kind.Function1](implicit i: Alternative[f#apply1]): Alternative[f#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Alternative[nt#oldtype1]): Alternative[nt#apply1] = new Alternative[nt#apply1] with ApplicativeProxy[nt#apply1] {
        private type f[+a] = nt#apply1[a]
        override val selfApplicative: selfApplicative = Applicative.deriving[nt]

        override def empty: empty = j.newOf(i.empty)
        override def op_<|>[a](x: f[a])(y: Lazy[f[a]]): f[a] = j.newOf(i.op_<|>(j.oldOf(x))(j.oldOf(y)))
        override def some[a](v: f[a]): f[List[a]] = j.newOf(i.some(j.oldOf(v)))
        override def many[a](v: f[a]): f[List[a]] = j.newOf(i.many(j.oldOf(v)))

        override def optional[a](x: f[a]): f[Maybe[a]] = j.newOf(i.optional(j.oldOf(x)))
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Alternative[nt#apply1]): Alternative[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}
