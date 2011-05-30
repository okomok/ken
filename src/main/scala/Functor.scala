

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor {
    type f_[a]
    def fmap[a, b](x: a => b)(y: f_[a]): f_[b]

    def op_<#>[a, b](x: a => b)(y: f_[a]): f_[b] = fmap(x)(y)
    def op_<#[a, b](x: => a)(y: f_[b]): f_[a] = fmap[b, a](_ => x)(y)

    private[ken] class Op_<#>[a, b](x: a => b) {
        def <#>(y: f_[a]): f_[b] = op_<#>(x)(y)
    }
    implicit def <#>[a, b](x: a => b): Op_<#>[a, b] = new Op_<#>(x)

    private[ken] class Op_<#[a](x: => a) {
        def <#[b](y: f_[b]): f_[a] = op_<#(x)(y)
    }
    implicit def <#[a](x: => a): Op_<#[a] = new Op_<#(x)
}
