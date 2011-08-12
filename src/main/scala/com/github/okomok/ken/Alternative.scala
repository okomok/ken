

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Alternative[f[+_]] extends Applicative[f] {
    final val asAlternative: Alternative[apply] = this

    // Core
    //
    def empty: f[Nothing]
    def op_<|>[a](x: f[a])(y: => f[a]): f[a]

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
    def optional[a](x: f[a]): f[Maybe[a]] = Just.of[a] <@> x <|> pure(Nothing.of[a])

    // Infix
    //
    sealed class Infix_<|>[a](x: f[a]) {
        def <|>(y: => f[a]): f[a] = op_<|>(x)(y)
    }
    final implicit def <|>[a](x: f[a]): Infix_<|>[a] = new Infix_<|>(x)
}


trait AlternativeProxy[f[+_]] extends Alternative[f] with ApplicativeProxy[f] {
    override def self: Alternative[f]

    override def empty: f[Nothing] = self.empty
    override def op_<|>[a](x: f[a])(y: => f[a]): f[a] = self.op_<|>(x)(y)
    override def some[a](v: f[a]): f[List[a]] = self.some(v)
    override def many[a](v: f[a]): f[List[a]] = self.many(v)

    override def optional[a](x: f[a]): f[Maybe[a]] = self.optional(x)
}


object Alternative {
    def apply[f <: Kind.Function1](implicit i: Alternative[f#apply]): Alternative[f#apply] = i
}
