

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monad extends Applicative {
    final def `return`[a](x: => a): f_[a] = pure(x)
    def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b]
    def op_>>[a, b](x: f_[a])(y: f_[b]): f_[b] = x >>= (_ => y)

    private[ken] class Op_>>=[a](x: f_[a]) {
        def >>=[b](y: a => f_[b]): f_[b] = op_>>=(x)(y)
    }
    implicit def >>=[a](x: f_[a]): Op_>>=[a] = new Op_>>=(x)

    private[ken] class Op_>>[a](x: f_[a]) {
        def >>[b](y: f_[b]): f_[b] = op_>>(x)(y)
    }
    implicit def >>[a, b](x: f_[a]): Op_>>[a] = new Op_>>(x)

    // will work only in generic context.
    private[ken] class ForExpr[a](x: f_[a]) {
       def map[b](y: a => b): f_[b] = op_>>=(x)(_x => pure(y(_x)))
       def flatMap[b](y: a => f_[b]): f_[b] = op_>>=(x)(y)
    }
    implicit def forExpr[a](x: f_[a]): ForExpr[a] = new ForExpr(x)

    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = {
        // x >>= (_x => y >>= (_y => pure(_x(_y))))
        for { _x <- x; _y <- y } yield _x(_y)
    }
}
