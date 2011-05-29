

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken
package detail


private[ken]
object _List extends Alternative[List] with MonadPlus[List] {
    // Functor
    override def fmap[a, b](x: a => b)(y: List[a]): List[b] = y match {
        case Nil => Nil
        case z :: zs => x(z) :: fmap(x)(zs)
    }
    // Applicative
    override def pure[a](x: => a): List[a] = x :: Nil
    override def op_<*>[a, b](x: List[a => b])(y: List[a]): List[b] = {
        for { p <- x; q <- y } yield p(q)
    }
    // Alternative
    override def empty[a]: List[a] = Nil
    override def op_<|>[a](x: List[a])(y: List[a]): List[a] = x ::: y
    // Monad
    override def op_>>=[a, b](x: List[a])(y: a => List[b]): List[b] = x.flatMap(y)
    // MonadPlus
    override def mzero[a]: List[a] = Nil
    override def mplus[a](x: List[a])(y: List[a]): List[a] = x ::: y
}


private[ken]
class _ListOf[a] extends Monoid[List[a]] {
    // Monoid
    override def mempty: List[a] = Nil
    override def mappend(x: List[a])(y: List[a]): List[a] = x ::: y
}
