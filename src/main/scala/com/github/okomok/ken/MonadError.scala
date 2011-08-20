

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadError[e, m[+_]] extends Monad[m] {
    final val asMonadError: MonadError[e, apply] = this

    // Core
    //
    def errorClass: ErrorClass[e]
    def throwError[a](e: e): m[a]
    def catchError[a](m: m[a])(h: e => m[a]): m[a]
}


trait MonadErrorProxy[e, m[+_]] extends MonadError[e, m] with MonadProxy[m] {
    def selfMonadError: MonadError[e, m]
    override def selfMonad: Monad[m] = selfMonadError

    override def errorClass: ErrorClass[e] = selfMonadError.errorClass
    override def throwError[a](e: e): m[a] = selfMonadError.throwError(e)
    override def catchError[a](m: m[a])(h: e => m[a]): m[a] = selfMonadError.catchError(m)(h)
}


object MonadError {
    def apply[e, m <: Kind.Function1](implicit i: MonadError[e, m#apply]): MonadError[e, m#apply] = i

    def deriving[e, nt <: Kind.Function1, ot <: Kind.Function1](implicit i: MonadError[e, ot#apply], j: Newtype1[nt#apply, ot#apply]): MonadError[e, nt#apply] = new MonadError[e, nt#apply] with MonadProxy[nt#apply] {
        private[this] type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt, ot](i, j)
        override def errorClass: ErrorClass[e] = i.errorClass
        override def throwError[a](e: e): m[a] = j.newOf { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = j.newOf { i.catchError(j.oldOf(m))(e => j.oldOf(h(e))) }
    }

    def weak[e, nt <: Kind.Newtype1](implicit i: MonadError[e, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadError[e, nt#oldtype1] = deriving[e, Kind.quote1[nt#oldtype1], nt](i, j.dual)
}
