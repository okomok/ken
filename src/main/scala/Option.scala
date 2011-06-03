

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/*
// Will be removed.

object Option_ extends Alternative[Option] with MonadPlus[Option] {
    private[this] type f[a] = Option[a]
    // Applicative
    override def pure[a](x: => a): f[a] = Some(x)
    // Alternative
    override def empty[a]: f[a] = None
    override def op_<|>[a](x: f[a])(y: f[a]): f[a] = (x, y) match {
        case (None, p) => p
        case (Some(p), _) => Some(p)
    }
    // Monad
    override def op_>>=[a, b](x: f[a])(y: a => f[b]): f[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f[a] = None
    override def mplus[a](x: f[a])(y: f[a]): f[a] = (x, y) match {
        case (None, None) => None
        case (Some(p), None) => Some(p)
        case (None, Some(p)) => Some(p)
        case (Some(p), Some(q)) => Some(p)
    }
}
*/
