

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Lifts a inner monad.
 */
trait MonadTrans[n[+_], m[+_]] extends TypeClass {
    def lift[a](n: n[a]): m[a]
}


object MonadTrans {
    def apply[n <: Metafunction1, m <: Metafunction1](implicit i: MonadTrans[n#apply, m#apply]): MonadTrans[n#apply, m#apply] = i
}


/*
trait MonadTrans[t[m[+_], +_]] extends TypeClass {
    def lift[m[+_], a](x: m[a])(implicit i: Monad[m]): t[m, a]
}


object MonadTrans {
    def apply[t[m[+_], +_]](implicit i: MonadTrans[t]): MonadTrans[t] = i
}
*/

