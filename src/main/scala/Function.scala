

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


class Function1Of[c] extends Applicative {
    // Functor
    override type f_[a] = c => a
    override def fmap[a, b](x: a => b)(y: f_[a]): c => b = z => x(y(z))
    // Applicative
    override def pure[a](x: => a): f_[a] = const(x)
    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): c => b = z => x(z)(y(z))
}


class Function1OfMonoid[a, b <: Monoid](val b: b) extends Monoid {
    // Monoid
    override type a_ = a => b.a_
    override def mempty: a_ = _ => b.mempty
    override def mappend(x: a_)(y: a_): a_ = z => b.mappend(x(z))(y(z))
}
