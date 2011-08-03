

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadError[e, m[+_]] extends Monad[m] {
    final def asMonadError: MonadError[e, apply] = this

    // Core
    //
    def errorClass: ErrorClass[e]
    def throwError[a](e: e): m[a]
    def catchError[a](m: m[a])(h: e => m[a]): m[a]
}


trait MonadErrorProxy[e, m[+_]] extends MonadError[e, m] with MonadProxy[m] {
    override def self: MonadError[e, m]

    override def errorClass: ErrorClass[e] = self.errorClass
    override def throwError[a](e: e): m[a] = self.throwError(e)
    override def catchError[a](m: m[a])(h: e => m[a]): m[a] = self.catchError(m)(h)
}


object MonadError {
    def apply[e, m[+_]](implicit i: MonadError[e, m]) = i
}
