

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[m[_]] extends Monad[m] {
    def mzero[a]: m[a]
    def mplus[a](x: m[a])(y: m[a]): m[a]
}


object MonadPlus {
    def mzero[m[_], a](implicit i: MonadPlus[m]): m[a] = i.mzero
    def mplus[m[_], a](x: m[a])(y: m[a])(implicit i: MonadPlus[m]): m[a] = i.mplus(x)(y)

    private[ken] class _Mplus_[m[_], a](x: m[a])(implicit i: MonadPlus[m]) {
        def _mplus_(y: m[a]): m[a] = mplus(x)(y)
    }
    implicit def _mplus_[m[_], a](x: m[a])(implicit i: MonadPlus[m]): _Mplus_[m, a] = new _Mplus_[m, a](x)

    def guard[m[_], a](b: Boolean)(implicit i: MonadPlus[m]): m[Unit] = b match {
        case true => i.`return`(())
        case false => i.mzero
    }

    def msum[m[_], a](xs: List[m[a]])(implicit i: MonadPlus[m]): m[a] = foldr(&.r(i.mplus[a]))(i.mzero)(xs)
}
