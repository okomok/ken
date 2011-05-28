

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object ListInstance extends MonadPlus {
    // Functor
    override type f_[a] = List[a]
    override def fmap[a, b](x: a => b)(y: f_[a]): f_[b] = y match {
        case Nil => Nil
        case z :: zs => x(z) :: fmap(x)(zs)
    }
    // Pointed
    override def pure[a](x: => a): f_[a] = x :: Nil
    // Applicative
    override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = {
        for { p <- x; q <- y } yield p(q)
    }
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: f_[a] = Nil
    override def op_mplus[a](x: f_[a])(y: f_[a]): f_[a] = x ::: y
}
