

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Alternative extends Applicative {
    def empty[a]: f_[a]
    def op_<|>[a](x: f_[a])(y: f_[a]): f_[a]

    def some[a](v: f_[a]): f_[List[a]] = {
        def many_v: f_[List[a]] = some_v <|> pure(List.Nil)
        def some_v: f_[List[a]] = _cons[a] <#> v <*> many_v
        some_v
    }

    def many[a](v: f_[a]): f_[List[a]] = {
        def many_v: f_[List[a]] = some_v <|> pure(List.Nil)
        def some_v: f_[List[a]] = _cons[a] <#> v <*> many_v
        many_v
    }

    private[ken] class Op_<|>[a](x: f_[a]) {
        def <|>(y: f_[a]): f_[a] = op_<|>(x)(y)
    }
    implicit def <|>[a](x: f_[a]): Op_<|>[a] = new Op_<|>(x)

    def optional[a](x: f_[a]): f_[Option[a]] = id[a => Option[a]](Some(_)) <#> x <|> pure(None)

    private def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}
