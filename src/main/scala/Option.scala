

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Option_ extends Alternative with MonadPlus {
    // Functor
    override type f_[a] = Option[a]
    // Applicative
    override def pure[a](x: => a): f_[a] = Some(x)
    // Alternative
    override def empty[a]: f_[a] = None
    override def op_<|>[a](x: f_[a])(y: f_[a]): f_[a] = (x, y) match {
        case (None, p) => p
        case (Some(p), _) => Some(p)
    }
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f_[a] = None
    override def mplus[a](x: f_[a])(y: f_[a]): f_[a] = (x, y) match {
        case (None, None) => None
        case (Some(p), None) => Some(p)
        case (None, Some(p)) => Some(p)
        case (Some(p), Some(q)) => Some(p)
    }
}
