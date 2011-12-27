

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadError[m[+_]] extends Monad[m] {
    final val asMonadError: MonadError[apply1] = this

    // Core
    //
    type ErrorType

    type throwError = ErrorType => m[Nothing]
    def throwError: ErrorType => m[Nothing] // throwError

    def catchError[a](m: m[a])(h: ErrorType => m[a]): m[a]
}


trait MonadErrorProxy[m[+_]] extends MonadError[m] with MonadProxy[m] {
    type selfMonadError = MonadError[m]
    val selfMonadError: selfMonadError
    override def selfMonad: selfMonad = selfMonadError

    override type ErrorType = selfMonadError.ErrorType
    override def throwError: throwError = selfMonadError.throwError
    override def catchError[a](m: m[a])(h: ErrorType => m[a]): m[a] = selfMonadError.catchError(m)(h)
}


object MonadError extends MonadErrorInstance {
    def apply[m <: Kind.Function1](implicit _M: MonadError[m#apply1]): MonadError[m#apply1] = _M

    def deriving[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadError[nt#oldtype1]): MonadError[nt#apply1] = new MonadError[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override type ErrorType = _M.ErrorType
        override def throwError: throwError = e => _Nt.newOf { _M.throwError(e) }
        override def catchError[a](m: m[a])(h: ErrorType => m[a]): m[a] = _Nt.newOf { _M.catchError(_Nt.oldOf(m))(e => _Nt.oldOf(h(e))) }
    }

    def weak[nt <: Kind.Newtype1](implicit _Nt: Newtype1[nt#apply1, nt#oldtype1], _M: MonadError[nt#apply1]): MonadError[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](_Nt.coNewtype, _M)
}


sealed trait MonadErrorInstance { this: MonadError.type =>
}
