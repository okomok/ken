

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object OptionInstance extends MonadPlus {
    // Functor
    override type f_[a] = Option[a]
    override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = y match {
        case None => None
        case Some(z) => Some(x(z))
    }
    // Pointed
    override def pure[a](x: => a): f_[a] = Some(x)
    // Applicative
    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = {
        for { p <- x; q <- y } yield p(q)
    }
    // Monad
    def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f_[a] = None
    override def op_mplus[a](x: f_[a])(y: f_[a]): f_[a] = (x, y) match {
        case (None, None) => None
        case (Some(p), None) => Some(p)
        case (None, Some(p)) => Some(p)
        case (Some(p), Some(q)) => Some(p)
    }
}
