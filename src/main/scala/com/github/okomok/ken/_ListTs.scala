

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


private[ken] final class _ListTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    // _ListT
    //
    final case class _ListT[+a](override val get: n[List[a]]) extends NewtypeOf[n[List[a]]]

    object _ListT extends MonadT[_ListT, n, List] with MonadPlus[_ListT] with ThisIsInstance {
        // Overrides
        //
        // Newtype1
        private type nt[+a] = _ListT[a]
        private type ot[+a] = n[List[a]]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _ListT(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        // Functor
        private type f[+a] = _ListT[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ListT {
            for { a <- run(m) } yield List.map(f)(a)
        }
        // Monad
        private type m[+a] = _ListT[a]
        override def `return`[a](a: Lazy[a]): m[a] = _ListT { inner.`return`(List(a.!)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ListT {
            for { a <- run(m); b <- inner.mapM(run[b]_ `.` k)(a) } yield List.concat(b)
        }
        // MonadPlus
        override def mzero: m[Nothing] = _ListT { inner.`return`(Nil) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _ListT {
            for { a <- run(m); b <- run(n) } yield a ++: b
        }
        // MonadTrans
        override def lift[a](n: n[a]): m[a] = _ListT {
            for { a <- n } yield List(a)
        }
    }
}
