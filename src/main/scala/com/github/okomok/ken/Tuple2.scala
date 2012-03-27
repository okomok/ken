

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// @aliasOf("Pair")
object Tuple2 extends Tuple2As with Kind.qcurry2[Tuple2] {

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
}

private[ken] sealed trait Tuple2As { this: Tuple2.type =>
    private[ken] def _asSemigroup[x, y](implicit sx: Semigroup[x], sy: Semigroup[y]): Semigroup[(x, y)] = new Semigroup[(x, y)] {
        private type a = (x, y)
        override val op_<>: : op_<>: = x1 => x2 => (x1, x2.!) match {
            case ((a1, b1), (a2, b2)) => (sx.op_<>:(a1)(a2), sy.op_<>:(b1)(b2))
        }
        override def times1p[n](n: n)(x0: a)(implicit j: Integral[n]): a = x0 match {
            case (a, b) => (sx.times1p(n)(a), sy.times1p(n)(b))
        }
    }

    private[ken] def _asMonoid[a, b](implicit ma: Monoid[a], mb: Monoid[b]): Monoid[(a, b)] = new Monoid[(a, b)] with SemigroupProxy[(a, b)] {
        override val selfSemigroup: selfSemigroup = _asSemigroup(ma, mb)
        override val mempty: mempty = (ma.mempty, mb.mempty)
        override val mappend: mappend = x1 => x2 => (x1, x2.!) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }

    private[ken] def _asApplicative[z](implicit ma: Monoid[z]): Applicative[apply[z]#apply1] with Extend[apply[z]#apply1] = new Applicative[apply[z]#apply1] with Extend[apply[z]#apply1] {
        // Applicative
        private type f[a] = (z, a)
        override def pure[a](x: Lazy[a]): f[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f[a => b]): f[a] => f[b] = a2 => (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }

    private[ken] def _asComonad[z]: Comonad[apply[z]#apply1] = new Comonad[apply[z]#apply1] {
        // Functor
        private type f[+a] = (z, a)
        override def fmap[a, b](f: a => b): f[a] => f[b] = { case (x, y) => (x, f(y)) }
        // Extend
        private type w[+a] = (z, a)
        override def duplicate[a](p: w[a]): w[w[a]] = (Pair.fst(p), p)
        // Comonad
        override def extract[a](p: w[a]): a = Pair.snd(p)
    }
}
