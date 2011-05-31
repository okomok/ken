

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[f[_]] extends Monad[f] {
    def mzero[a]: f[a]
    def mplus[a](x: f[a])(y: f[a]): f[a]

    private[ken] class _Mplus_[a](x: f[a]) {
        def _mplus_(y: f[a]): f[a] = mplus(x)(y)
    }
    implicit def _mplus_[a](x: f[a]): _Mplus_[a] = new _Mplus_(x)
}
