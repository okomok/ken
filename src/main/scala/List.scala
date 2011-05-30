

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object List_ extends Alternative with MonadPlus {
    // Functor
    override type f_[a] = List[a]
    override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = y match {
        case Nil => Nil
        case z :: zs => x(z) :: fmap(x)(zs)
    }
    // Applicative
    override def pure[a](x: => a): f_[a] = x :: Nil
    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = {
        for { p <- x; q <- y } yield p(q)
    }
    // Alternative
    override def empty[a]: f_[a] = Nil
    override def op_<|>[a](x: f_[a])(y: f_[a]): f_[a] = x ::: y
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f_[a] = Nil
    override def mplus[a](x: f_[a])(y: f_[a]): f_[a] = x ::: y
}

class ListOf[a] extends Monoid {
    // Monoid
    override type m_ = List[a]
    override def mempty: m_ = Nil
    override def mappend(x: m_)(y: m_): m_ = x ::: y
}
