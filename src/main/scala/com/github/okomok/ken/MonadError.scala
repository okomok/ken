

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadError[e, m[+_]] extends Monad[m] {
    final val asMonadError: MonadError[e, apply] = this

    // Core
    //
    def throwError[a](e: e): m[a]
    def catchError[a](m: m[a])(h: e => m[a]): m[a]
}


trait MonadErrorProxy[e, m[+_]] extends MonadError[e, m] with MonadProxy[m] {
    def selfMonadError: MonadError[e, m]
    override def selfMonad: Monad[m] = selfMonadError

    override def throwError[a](e: e): m[a] = selfMonadError.throwError(e)
    override def catchError[a](m: m[a])(h: e => m[a]): m[a] = selfMonadError.catchError(m)(h)
}


object MonadError extends MonadErrorInstance {
    def apply[e, m <: Kind.Function1](implicit i: MonadError[e, m#apply]): MonadError[e, m#apply] = i

    def deriving[e, nt <: Kind.Newtype1](implicit i: MonadError[e, nt#oldtype1], j: Newtype1[nt#apply, nt#oldtype1]): MonadError[e, nt#apply] = new MonadError[e, nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt]

        override def throwError[a](e: e): m[a] = j.newOf { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = j.newOf { i.catchError(j.oldOf(m))(e => j.oldOf(h(e))) }
    }

    def derivingT[e, mt <: Kind.MonadT](implicit i: MonadError[e, mt#innerMonad], j: MonadT[mt#apply, mt#innerMonad, mt#baseMonad]): MonadError[e, mt#apply] = new MonadError[e, mt#apply] with MonadProxy[mt#apply] {
        private type m[+a] = mt#apply[a]
        override def selfMonad = j

        override def throwError[a](e: e): m[a] = j.lift { i.throwError(e) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = j.newOf { i.catchError(j.oldOf(m))(e => j.oldOf(h(e))) }
    }

    def weak[e, nt <: Kind.Newtype1](implicit i: MonadError[e, nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): MonadError[e, nt#oldtype1] = deriving[e, Kind.coNewtype1[nt]](i, j.coNewtype)
}


sealed trait MonadErrorInstance { this: MonadError.type =>
    implicit def ofMonadT[e, mt <: Kind.MonadT](implicit i: MonadError[e, mt#innerMonad], j: MonadT[mt#apply, mt#innerMonad, mt#baseMonad]): MonadError[e, mt#apply] = derivingT(i, j)
}
