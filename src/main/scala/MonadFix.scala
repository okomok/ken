

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadFix[m[+_]] extends Monad[m] { outer =>
    def mfix[a](f: (=> a) => m[a]): m[a]
}

trait MonadFixProxy[m[+_]] extends MonadFix[m] with MonadProxy[m] {
    override def self: MonadFix[m]
    override def mfix[a](f: (=> a) => m[a]): m[a] = self.mfix(f)
}