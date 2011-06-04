

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[m[_]] extends Monad[m] {
    def mzero[a]: m[a]
    def mplus[a](x: m[a])(y: m[a]): m[a]

    private[ken] class _Mplus_[a](x: m[a]) {
        def _mplus_(y: m[a]): m[a] = mplus(x)(y)
    }
    implicit def _mplus_[a](x: m[a]): _Mplus_[a] = new _Mplus_(x)

    final def guard(b: Boolean): m[Unit] = b match {
        case true => `return`(())
        case false => mzero
    }

    final def msum[a](xs: List[m[a]]): m[a] = foldr(&.r(mplus[a]))(mzero)(xs)
}
