

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/*
object Pair_ {

    class PairOf[A] extends Functor {
        override type f_[a] = (A, a)
        override def fmap[a, b](f: a => b)(a2: f_[a]): f_[b] = a2 match {
            case (x, y) => (x, f(y))
        }
    }

    class PairOfMonoid[Ma <: Monoid](val ma: Ma) extends Applicative {
        override type f_[a] = (ma.m_, a)
        override def pure[a](x: => a): f_[a] = (ma.mempty, x)
        override def op_<*>[a, b](a1: f_[a => b])(a2: f_[a]): f_[b] = (a1, a2) match {
            case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
        }
    }

    class PairOfMonoids[Ma <: Monoid, Mb <: Monoid](val ma: Ma, val mb: Mb) extends Monoid {
        type m_ = (ma.m_, mb.m_)
        def mempty: m_ = (ma.mempty, mb.mempty)
        def mappend(x1: m_)(x2: m_): m_ = (x1, x2) match {
            case ((a1, b1), (a2, b2)) => (ma.mappend(a1)(a2), mb.mappend(b1)(b2))
        }
    }

}
*/
