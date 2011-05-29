

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


trait Alternative[f[_]] extends Applicative[f] {
    def empty[a]: f[a]
    def op_<|>[a](x: f[a])(y: f[a]): f[a]

    def some[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <#> v <*> many_v
        some_v
    }

    def many[a](v: f[a]): f[List[a]] = {
        def many_v: f[List[a]] = some_v <|> pure(Nil)
        def some_v: f[List[a]] = _cons[a] <#> v <*> many_v
        many_v
    }

    private[ken] class _Op_<|>[a](x: f[a]) {
        def <|>(y: f[a]): f[a] = op_<|>(x)(y)
    }
    implicit def <|>[a](x: f[a]): _Op_<|>[a] = new _Op_<|>(x)

    def optional[a](x: f[a]): f[Option[a]] = id[a => Option[a]](Some(_)) <#> x <|> pure(None)

    private def _cons[a]: a => List[a] => List[a] = x => xs => x :: xs
}


object Alternative {
    implicit val Option: Alternative[Option] = detail._Option
    implicit val List: Alternative[List] = detail._List
/*
    def optional[f[_], a](x: f[a])(implicit m: Alternative[f]): f[Option[a]] = {
        import m._
        (Some(_: a): Option[a]) <#> x <|> pure(None)
    }
*/
}
