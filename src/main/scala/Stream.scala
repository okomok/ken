

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Stream_ extends Alternative with MonadPlus {
    // Functor
    override type f_[a] = Stream[a]
    // Applicative
    override def pure[a](x: => a): f_[a] = Stream.cons(x, Stream.Empty)
    // Alternative
    override def empty[a]: f_[a] = Stream.Empty
    override def op_<|>[a](x: f_[a])(y: f_[a]): f_[a] = x append y
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f_[a] = Stream.Empty
    override def mplus[a](x: f_[a])(y: f_[a]): f_[a] = x append y
}

class StreamOf[a] extends Monoid {
    // Monoid
    override type m_ = Stream[a]
    override def mempty: m_ = Stream.Empty
    override def mappend(x: m_)(y: m_): m_ = x append y
}


object ZipStream extends Applicative {
    // Functor
    override type f_[a] = Stream[a]
    // Applicative
    override def pure[a](x: => a): f_[a] = Stream.continually(x)
    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = x.zip(y).map{case (x1, x2) => x1(x2)}
}
