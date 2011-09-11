

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Tuple2 extends Kind.qcurry2[Tuple2] {

    // Prelude
    //
    def fst[a](p: (a, _)): a = p match {
        case (x, _) => x
    }

    def snd[b](p: (_, b)): b = p match {
        case (_, y) => y
    }

    def curry[a, b, c](f: Pair[a, b] => c): a => b => c = x => y => f((x, y))

    def uncurry[a, b, c](f: a => b => c): Pair[a, b] => c = { case (x, y) => f(x)(y) }

    def swap[a, b](p: (a, b)): (b, a) = p match {
        case (x, y) => (y, x)
    }

    // Instances
    //
    private[ken] def _asMonoid[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] {
        override val mempty: mempty = (ma.mempty, mb.mempty)
        override val mappend: mappend = x1 => x2 => (x1, x2.!) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }

    private[ken] def _asApplicative[z](implicit ma: Monoid[z]): Applicative[({type f[+a] = (z, a)})#f] = new Applicative[({type f[+a] = (z, a)})#f] {
        private type f[a] = (z, a)
        override def pure[a](x: Lazy[a]): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }
}
