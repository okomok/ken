

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


private[ken] final class _LazyTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    // _LazyT
    //
    final case class _LazyT[+a](override val get: n[Lazy[a]]) extends NewtypeOf[n[Lazy[a]]]

    object _LazyT extends MonadT[_LazyT, n, Lazy] with ThisIsInstance {
        // Overrides
        //
        // Newtype1
        private type nt[+a] = _LazyT[a]
        private type ot[+a] = n[Lazy[a]]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _LazyT(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        // Functor
        private type f[+a] = _LazyT[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _LazyT {
            for { a <- run(m) } yield Lazy(f(a.!))
        }
        // Monad
        private type m[+a] = _LazyT[a]
        override def `return`[a](a: Lazy[a]): m[a] = _LazyT { inner.`return`(Lazy(a)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _LazyT {
            for { a <- run(m); * <- run(k(a.!)) } yield *
        }
        // MonadT
        /*private*/ type u[+a] = Lazy[a]
        override val innerMonad: Monad[n] = inner
        override val baseMonad: Monad[u] = Monad[Lazy.type]
    }
}
