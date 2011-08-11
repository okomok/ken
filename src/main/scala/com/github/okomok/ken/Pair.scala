

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Pair extends Kind.qcurry2[Pair] {
    def _asMonoid[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        private[this] type m = (a, b)
        override val mempty: m = (ma.mempty, mb.mempty)
        override val mappend: m => (=> m) => m = { x1 => x2 => (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        } }
    }

    def _asApplicative[z](implicit ma: Monoid[z]): Applicative[({type f[+a] = (z, a)})#f] = new Applicative[({type f[+a] = (z, a)})#f] {
        private[this] type f[a] = (z, a)
        override def pure[a](x: => a): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
