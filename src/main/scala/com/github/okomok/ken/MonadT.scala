

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Monad-Transformer (stateless only...)
//
trait MonadT[m[+_], n[+_], u[+_]] extends Newtype1[m, ({type ot[+a] = n[u[a]]})#ot]
    with Monad[m] with MonadTrans[m, n] with Kind.MonadT
{
    override type baseMonad[+a] = u[a]

    final val asMonadT: MonadT[m, n, u] = this

    def map[n_[+_], a, b](f: n[u[a]] => n_[u[b]])(n: m[a]): NewtypeOf[n_[u[b]]] = NewtypeOf { f(oldOf(n)) }
    implicit def dependent[a](n: NewtypeOf[n[u[a]]]): m[a] = newOf { n.get }
}


trait MonadTProxy[m[+_], n[+_], u[+_]] extends Newtype1Proxy[m, ({type ot[+a] = n[u[a]]})#ot]
    with MonadT[m, n, u] with MonadProxy[m] with MonadTransProxy[m, n]
{
    def selfMonadT: MonadT[m, n, u]
    override def selfNewtype1: Newtype1[m, ({type ot[+a] = n[u[a]]})#ot] = selfMonadT
    override def selfMonad: Monad[m] = selfMonadT
    override def selfMonadTrans: MonadTrans[m, n] = selfMonadT

    override def map[n_[+_], a, b](f: n[u[a]] => n_[u[b]])(n: m[a]): NewtypeOf[n_[u[b]]] = selfMonadT.map(f)(n)
    override def dependent[a](n: NewtypeOf[n[u[a]]]): m[a] = selfMonadT.dependent(n)
}


object MonadT {
    def apply[mt <: Kind.MonadT](implicit i: MonadT[mt#apply1, mt#innerMonad, mt#baseMonad]): MonadT[mt#apply1, mt#innerMonad, mt#baseMonad] = i
}
