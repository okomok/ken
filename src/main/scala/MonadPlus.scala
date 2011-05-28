

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus extends Monad {
    def mzero[a]: f_[a]
    def op_mplus[a](x: f_[a])(y: f_[a]): f_[a]

    class Op_mplus[a](x: f_[a]) {
        def mplus(y: f_[a]): f_[a] = op_mplus(x)(y)
    }
    implicit def mplus[a](x: f_[a]): Op_mplus[a] = new Op_mplus(x)
}
