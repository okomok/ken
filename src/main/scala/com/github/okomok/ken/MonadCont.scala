

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadCont[m[+_]] extends Monad[m] {
    final def asMonadCont: MonadCont[apply] = this

    // Core
    //
    def callCC[a, b](f: (a => m[b]) => m[a]): m[a]
}


trait MonadContProxy[m[+_]] extends MonadCont[m] with MonadProxy[m] {
    override def self: MonadCont[m]

    override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = self.callCC(f)
}


object MonadCont {
    def apply[m[+_]](implicit i: MonadCont[m]) = i
}
