

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monad extends Applicative {
    def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b]
    def op_>>[a, b](x: f_[a])(y: f_[b]): f_[b] = x >>= (_ => y)

    class Op_>>=[a](x: f_[a]) {
        def >>=[b](y: a => f_[b]): f_[b] = op_>>=(x)(y)
    }
    implicit def >>=[a](x: f_[a]): Op_>>=[a] = new Op_>>=(x)

    class Op_>>[a](x: f_[a]) {
        def >>[b](y: f_[b]): f_[b] = op_>>(x)(y)
    }
    implicit def >>[a, b](x: f_[a]): Op_>>[a] = new Op_>>(x)
}
