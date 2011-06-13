

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Alternative[f[_]] extends Applicative[f] {
    import Alternative.{<|>}
    import Applicative.{<@>, <*>}
    private[this] implicit val i = this

    def empty[a]: f[a]
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

    private[this] def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}


object Alternative {
    import Applicative._

    def empty[f[_], a](implicit i: Alternative[f]): f[a] = i.empty[a]
    def op_<|>[f[_], a](x: f[a])(y: => f[a])(implicit i: Alternative[f]): f[a] = i.op_<|>(x)(y)

    def some[f[_], a](v: f[a])(implicit i: Alternative[f]): f[List[a]] = i.some(v)
    def many[f[_], a](v: f[a])(implicit i: Alternative[f]): f[List[a]] = i.many(v)

    private[ken] class Op_<|>[f[_], a](x: f[a])(implicit i: Alternative[f]) {
        def <|>(y: => f[a]): f[a] = op_<|>(x)(y)
    }
    implicit def <|>[f[_], a](x: f[a])(implicit i: Alternative[f]): Op_<|>[f, a] = new Op_<|>[f, a](x)

    def optional[f[_], a](x: f[a])(implicit i: Alternative[f]): f[Maybe[a]] = (Just(_: a).of[a]) <@> x <|> pure(Nothing.of[a])(i)
}
