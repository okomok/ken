

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Alternative[f[_]] extends Applicative[f] {
    def empty[a]: f[a]
    def op_<|>[a](x: f[a])(y: f[a]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(List.Nil)
        def some_v: f[List[a]] = _cons[a] <#> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(List.Nil)
        def some_v: f[List[a]] = _cons[a] <#> v <*> many_v
        many_v
    }

    private[ken] class Op_<|>[a](x: f[a]) {
        def <|>(y: f[a]): f[a] = op_<|>(x)(y)
    }
    implicit def <|>[a](x: f[a]): Op_<|>[a] = new Op_<|>(x)

    def optional[a](x: f[a]): f[Option[a]] = id[a => Option[a]](Some(_)) <#> x <|> pure(None)

    private[this] def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}
