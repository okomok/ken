

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Alternative[f[+_]] extends Applicative[f] { outer =>
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

    override implicit def method[a](x: f[a]): AlternativeMethod[f, a] = new AlternativeMethod[f, a] {
        override def klass = outer
        override def callee = x
    }

    final def optional[a](x: f[a]): f[Maybe[a]] = (Just(_: a).up) <@> x <|> pure(Nothing.of[a])

    private[this] def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}

trait AlternativeMethod[f[+_], +a] extends ApplicativeMethod[f, a] {
    override def klass: Alternative[f]
    final def <|>[b >: a](y: => f[b]): f[b] = klass.op_<|>[b](callee)(y)
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
