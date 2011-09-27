

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// MaybeT + reason == ErrorT


private[ken] final class _MaybeTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    // _MaybeT
    //
    final case class _MaybeT[+a](override val get: n[Maybe[a]]) extends NewtypeOf[n[Maybe[a]]]

    object _MaybeT extends MonadT[_MaybeT, n, Maybe] with MonadPlus[_MaybeT] with ThisIsInstance {
        // Overrides
        //
        // Newtype1
        private type nt[+a] = _MaybeT[a]
        private type ot[+a] = n[Maybe[a]]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _MaybeT(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        // Monad
        private type m[+a] = _MaybeT[a]
        override def `return`[a](a: Lazy[a]): m[a] = _MaybeT { inner.`return`(Just(a.!).up) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _MaybeT {
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => inner.`return`(Nothing.of[b])
                    case Just(r) => run(k(r))
                }
            } yield *
        }
        // MonadPlus
        override def mzero: m[Nothing] = _MaybeT { inner.`return`(Nothing) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _MaybeT {
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => run(n)
                    case Just(r) => inner.`return`(Just(r))
                }
            } yield *
        }
        // MonadT
        /*private*/ type u[+a] = Maybe[a]
        override val innerMonad: Monad[n] = inner
        override val baseMonad: Monad[u] = Monad[Maybe.type]
    }
}
