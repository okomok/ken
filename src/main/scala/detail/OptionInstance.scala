

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken
package detail


private[ken]
object OptionInstance extends Alternative[Option] with MonadPlus[Option] {
    // Functor
    override def fmap[a, b](x: a => b)(y: Option[a]): Option[b] = y match {
        case None => None
        case Some(z) => Some(x(z))
    }
    // Applicative
    override def pure[a](x: => a): Option[a] = Some(x)
    override def op_<*>[a, b](x: Option[a => b])(y: Option[a]): Option[b] = {
        for { p <- x; q <- y } yield p(q)
    }
    // Alternative
    override def empty[a]: Option[a] = None
    override def op_<|>[a](x: Option[a])(y: Option[a]): Option[a] = (x, y) match {
        case (None, p) => p
        case (Some(p), _) => Some(p)
    }
    // Monad
    def op_>>=[a, b](x: Option[a])(y: a => Option[b]): Option[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: Option[a] = None
    override def op_mplus[a](x: Option[a])(y: Option[a]): Option[a] = (x, y) match {
        case (None, None) => None
        case (Some(p), None) => Some(p)
        case (None, Some(p)) => Some(p)
        case (Some(p), Some(q)) => Some(p)
    }
}
