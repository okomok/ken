

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


class Function1Of[A] extends Applicative[({type f[a] = A => a})#f] {
    private[this] type f[a] = A => a
    // Functor
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = z => x(y(z))
    // Applicative
    override def pure[a](x: => a): f[a] = const(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = z => x(z)(y(z))
}
/*
class Function1OfMonoid[A, Mb <: Monoid](val mb: Mb) extends Monoid {
    override type m_ = A => mb.m_
    override def mempty: m_ = _ => mb.mempty
    override def mappend(x: m_)(y: m_): m_ = z => mb.mappend(x(z))(y(z))
}
*/
