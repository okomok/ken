

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken

/*
final class Type1[m] {

    case class Const[b](getConst: m)

    object Const {
        implicit object functorInstance extends Functor[Const] {
            private[this] type f[x] = Const[x]
            override def fmap[a, b](x: a => b)(y: f[a]): f[b] = y match {
                case Const(v) => Const(v)
            }
        }

        implicit def applicativeInstance(implicit i: Monoid[m]): Applicative[Const] = new Applicative[Const] {
            private[this] type f[x] = Const[x]
            override def pure[a](x: => a): f[a] = Const(i.mempty)
            override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = (x, y) match {
                case (Const(f), Const(v)) => Const(i.mappend(f)(v))
            }
        }
    }

    /*
    case class Func1[r](getFunc: m => r)

    object Func1 {
        implicit def applicative[x]: Applicative[Function1] = new Applicative[m => x] {
            private[this] type f[x] = m => x
            override def pure[a](x: => a): f[a] = const(x)
            override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = z => x(z)(y(z))
        }

        implicit def ofMonoid[a](implicit ma: Monoid[a]): Applicative[({type f[x] = (a, x)})#f] = new Applicative[({type f[x] = (a, x)})#f] {
            private[this] type f[x] = (a, x)
            override def pure[a](x: => a): f[a] = (ma.mempty, x)
            override def op_<*>[a, b](a1: f[a => b])(a2: f[a]): f[b] = (a1, a2) match {
                case ((u, f), (v, x)) => (ma.mappend(u)(v), f(x))
            }
        }
    }
    */
}
*/
