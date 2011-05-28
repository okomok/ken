

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[f[_]] extends Monad[f] {
    def mzero[a]: f[a]
    def op_mplus[a](x: f[a])(y: f[a]): f[a]

    class _Mplus[a](x: f[a]) {
        def mplus(y: f[a]): f[a] = op_mplus(x)(y)
    }
    implicit def mplus[a](x: f[a]): _Mplus[a] = new _Mplus(x)
}


object MonadPlus {
    implicit val Option: MonadPlus[Option] = detail.OptionInstance
    implicit val List: MonadPlus[List] = detail.ListInstance
}
