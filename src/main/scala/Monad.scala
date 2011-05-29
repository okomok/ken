

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monad[f[_]] extends Applicative[f] {
    def op_>>=[a, b](x: f[a])(y: a => f[b]): f[b]
    def op_>>[a, b](x: f[a])(y: f[b]): f[b] = x >>= (_ => y)

    private[ken] class _Op_>>=[a](x: f[a]) {
        def >>=[b](y: a => f[b]): f[b] = op_>>=(x)(y)
    }
    implicit def >>=[a](x: f[a]): _Op_>>=[a] = new _Op_>>=(x)

    private[ken] class _Op_>>[a](x: f[a]) {
        def >>[b](y: f[b]): f[b] = op_>>(x)(y)
    }
    implicit def >>[a, b](x: f[a]): _Op_>>[a] = new _Op_>>(x)
}


object Monad {
    implicit val Option: Monad[Option] = detail._Option
    implicit val List: Monad[List] = detail._List
}
