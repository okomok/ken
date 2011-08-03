

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Unused


trait MonadTrans[t[m[+_], +_]] extends TypeClass {
    def lift[m[+_], a](x: m[a])(implicit i: Monad[m]): t[m, a]
}


object MonadTrans {
    def apply[t[m[+_], +_]](implicit i: MonadTrans[t]): MonadTrans[t] = i
}
