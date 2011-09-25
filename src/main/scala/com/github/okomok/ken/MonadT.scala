

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Monad-Transformer (stateless only...)
//
trait MonadT[m[+_], n[+_], u[+_]] extends Monad[m] with Kind.MonadT {
    override type innerMonad[+a] = n[a]
    override type baseMonad[+a] = u[a]

    final val asMonadT: MonadT[m, n, u] = this

    // Core
    //
    def lift[a](n: n[a]): m[a]
    def newOf[a](n: n[u[a]]): m[a]
    def oldOf[a](m: m[a]): n[u[a]]
}


trait MonadTProxy[m[+_], n[+_], u[+_]] extends MonadT[m, n, u] with MonadProxy[m] {
    def selfMonadT: MonadT[m, n, u]
    override def selfMonad: Monad[m] = selfMonadT

    override def lift[a](n: n[a]): m[a] = selfMonadT.lift(n)
    override def newOf[a](n: n[u[a]]): m[a] = selfMonadT.newOf(n)
    override def oldOf[a](m: m[a]): n[u[a]] = selfMonadT.oldOf(m)
}


object MonadT {
    def apply[mt <: Kind.MonadT](implicit i: MonadT[mt#apply1, mt#innerMonad, mt#baseMonad]): MonadT[mt#apply1, mt#innerMonad, mt#baseMonad] = i
}
