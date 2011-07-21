

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Alternative[f[+_]] extends Applicative[f] {
// Overridables
    def empty: f[Nothing]
    def op_<|>[a](x: f[a])(y: => f[a]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <@> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <@> v <*> many_v
        many_v
    }

// Utilities
    final def optional[a](x: f[a]): f[Maybe[a]] = (Just(_: a).up) <@> x <|> pure(Nothing.of[a])

    private[this] def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs

// Infix Operators
    sealed class Infix_<|>[a](x: f[a]) {
        def <|>(y: => f[a]): f[a] = op_<|>(x)(y)
    }
    implicit def <|>[a](x: f[a]): Infix_<|>[a] = new Infix_<|>(x)
}


trait AlternativeProxy[f[+_]] extends Alternative[f] with ApplicativeProxy[f] {
    override def self: Alternative[f]
    override def empty: f[Nothing] = self.empty
    override def op_<|>[a](x: f[a])(y: => f[a]): f[a] = self.op_<|>(x)(y)
    override def some[a](v: f[a]): f[List[a]] = self.some(v)
    override def many[a](v: f[a]): f[List[a]] = self.many(v)
}


object Alternative {
    def apply[f[+_]](implicit i: Alternative[f]): Alternative[f] = i
}
