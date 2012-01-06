

// Copyright Shunsuke Sogame 2011.
//
// https://github.com/pepeiborra/control-monad-free
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadFree[f[+_], m[+_]] extends Monad[m] with Kind.MonadFree {
    override type freeFunctor[+a] = f[a]
    final val asMonadFree: MonadFree[freeFunctor, apply1] = this

    // Core
    //
    type functor = Functor[f]
    val functor: functor

    def free[a, b](m: m[a]): m[Either[a, f[m[a]]]]

    def wrap[a](f: f[m[a]]): m[a]
}


trait MonadFreeProxy[f[+_], m[+_]] extends MonadFree[f, m] with MonadProxy[m] {
    type selfMonadFree = MonadFree[f, m]
    def selfMonadFree: selfMonadFree
    override def selfMonad: selfMonad = selfMonadFree

    override val functor: functor = selfMonadFree.functor
    override def free[a, b](m: m[a]): m[Either[a, f[m[a]]]] = selfMonadFree.free(m)
    override def wrap[a](f: f[m[a]]): m[a] = selfMonadFree.wrap(f)
}


object MonadFree extends MonadFreeInstance {
    def apply[f <: Kind.Function1, m <: Kind.Function1](implicit i: MonadFree[f#apply1, m#apply1]): MonadFree[f#apply1, m#apply1] = i
}


sealed trait MonadFreeInstance { this: MonadFree.type =>
}
