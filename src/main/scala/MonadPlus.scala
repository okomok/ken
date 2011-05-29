

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus extends Monad {
    def mzero[a]: f_[a]
    def mplus[a](x: f_[a])(y: f_[a]): f_[a]

    private[ken] class _Mplus_[a](x: f_[a]) {
        def _mplus_(y: f_[a]): f_[a] = mplus(x)(y)
    }
    implicit def _mplus_[a](x: f_[a]): _Mplus_[a] = new _Mplus_(x)
}
