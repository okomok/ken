

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[_]] {
    def fmap[a, b](x: a => b)(y: f[a]): f[b]

    def op_<#>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
    def op_<#[a, b](x: => a)(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

    class _Op_<#>[a, b](x: a => b) {
        def <#>(y: f[a]): f[b] = op_<#>(x)(y)
    }
    implicit def <#>[a, b](x: a => b): _Op_<#>[a, b] = new _Op_<#>(x)

    class _Op_<#[a](x: => a) {
        def <#[b](y: f[b]): f[a] = op_<#(x)(y)
    }
    implicit def <#[a](x: => a): _Op_<#[a] = new _Op_<#(x)
}


object Functor {
    implicit val Option: Functor[Option] = detail.OptionInstance
    implicit val List: Functor[List] = detail.ListInstance
}
