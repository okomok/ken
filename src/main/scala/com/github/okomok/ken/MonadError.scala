

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadError[e, m[+_]] extends Monad[m] {
    final val asMonadError: MonadError[e, apply] = this

    // Core
    //
    type throwError = e => m[Nothing]
    def throwError: throwError

    def catchError[a](m: m[a])(h: e => m[a]): m[a]
}


trait MonadErrorProxy[e, m[+_]] extends MonadError[e, m] with MonadProxy[m] {
    type selfMonadError = MonadError[e, m]
    def selfMonadError: selfMonadError
    override def selfMonad: selfMonad = selfMonadError

    override def throwError: throwError = selfMonadError.throwError
    override def catchError[a](m: m[a])(h: e => m[a]): m[a] = selfMonadError.catchError(m)(h)
}


object MonadError extends MonadErrorInstance {
    def apply[e, m <: Kind.Function1](implicit i: MonadError[e, m#apply1]): MonadError[e, m#apply1] = i

    def deriving[e, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadError[e, nt#oldtype1]): MonadError[e, nt#apply1] = new MonadError[e, nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override def throwError: throwError = e => j.newOf { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = j.newOf { i.catchError(j.oldOf(m))(e => j.oldOf(h(e))) }
    }

    def weak[e, nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadError[e, nt#apply1]): MonadError[e, nt#oldtype1] = deriving[e, Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadErrorInstance { this: MonadError.type =>
}
