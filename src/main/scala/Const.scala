

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


case class Const[a, b](getConst: a)


object Const {
    implicit def functor[m]: Functor[({type f[x] = Const[m, x]})#f] = new Functor[({type f[x] = Const[m, x]})#f] {
        private[this] type f[x] = Const[m, x]
        override def fmap[a, b](x: a => b)(y: f[a]): f[b] = y match {
            case Const(v) => Const(v)
        }
    }

    implicit def applicative[m](implicit i: Monoid[m]): Applicative[({type f[x] = Const[m, x]})#f] = new Applicative[({type f[x] = Const[m, x]})#f] {
        private[this] type f[x] = Const[m, x]
        override def pure[a](x: => a): f[a] = Const(i.mempty)
        override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = (x, y) match {
            case (Const(f), Const(v)) => Const(i.mappend(f)(v))
        }
    }
}
