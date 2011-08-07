

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Lifts an inner _monad.
 */
trait MonadTrans[n[+_], m[+_]] extends Typeclass {
    def lift[a](n: n[a]): m[a]
}


object MonadTrans {
    def apply[m <: Kind.MonadTrans](implicit i: MonadTrans[m#inner, m#apply]): MonadTrans[m#inner, m#apply] = i
}
