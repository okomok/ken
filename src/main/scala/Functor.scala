

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[_]] {
    type ftype[a] = f[a]
    def fmap[a, b](x: a => b)(y: f[a]): f[b]

    final def op_<#>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
    def op_<#[a, b](x: => a)(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

    private[ken] class Op_<#>[a, b](x: a => b) {
        def <#>(y: f[a]): f[b] = op_<#>(x)(y)
    }
    implicit def <#>[a, b](x: a => b): Op_<#>[a, b] = new Op_<#>(x)

    private[ken] class Op_<#[a](x: => a) {
        def <#[b](y: f[b]): f[a] = op_<#(x)(y)
    }
    implicit def <#[a](x: => a): Op_<#[a] = new Op_<#(x)
}


object Functor extends ApplicativeInstance
