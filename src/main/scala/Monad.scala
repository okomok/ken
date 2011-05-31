

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monad[f[_]] extends Applicative[f] {
    final def `return`[a](x: => a): f[a] = pure(x)
    def op_>>=[a, b](x: f[a])(y: a => f[b]): f[b]
    def op_>>[a, b](x: f[a])(y: f[b]): f[b] = x >>= (_ => y)

    private[ken] class Op_>>=[a](x: f[a]) {
        def >>=[b](y: a => f[b]): f[b] = op_>>=(x)(y)
    }
    implicit def >>=[a](x: f[a]): Op_>>=[a] = new Op_>>=(x)

    private[ken] class Op_>>[a](x: f[a]) {
        def >>[b](y: f[b]): f[b] = op_>>(x)(y)
    }
    implicit def >>[a, b](x: f[a]): Op_>>[a] = new Op_>>(x)

    // will work only in generic context.
    private[ken] class ForExpr[a](x: f[a]) {
       def map[b](y: a => b): f[b] = op_>>=(x)(_x => pure(y(_x)))
       def flatMap[b](y: a => f[b]): f[b] = op_>>=(x)(y)
    }
    implicit def forExpr[a](x: f[a]): ForExpr[a] = new ForExpr(x)

    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = {
        // x >>= (_x => y >>= (_y => pure(_x(_y))))
        for { _x <- x; _y <- y } yield _x(_y)
    }
}
