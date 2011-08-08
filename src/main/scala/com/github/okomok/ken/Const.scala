

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final case class Const[a, +b](override val get: a) extends Strong[a]


object Const extends Kind.Function1nv {
    sealed trait apply[z] extends Kind.Strong1 {
        override type apply[+a] = Const[z, a]
        override type weak[+a] = z
    }

    implicit def weak[z]: Imply1[({type p[+a] = Const[z, a]})#p, ({type d[+a] = z})#d] = new Imply1[({type p[+a] = Const[z, a]})#p, ({type d[+a] = z})#d] {
        private[this] type p[+a] = Const[z, a]
        private[this] type d[+a] = z
        override def imply1[a](p: p[a]): d[a] = p.get
        override def unimply1[a](d: => d[a]): p[a] = Const(d)
    }

    implicit def _asFunctor[z]: Functor[({type f[+a] = Const[z, a]})#f] = new Functor[({type f[+a] = Const[z, a]})#f] {
        private[this] type f[+a] = Const[z, a]
        override def fmap[a, b](a: a => b)(y: f[a]): f[b] = y match {
            case Const(v) => Const(v)
        }
    }

    implicit def _asApplicative[z](implicit i: Monoid[z]): Applicative[({type f[+a] = Const[z, a]})#f] = new Applicative[({type f[+a] = Const[z, a]})#f] {
        private[this] type f[+a] = Const[z, a]
        override def pure[a](a: => a): f[a] = Const(i.mempty)
        override def op_<*>[a, b](a: f[a => b])(y: f[a]): f[b] = (a, y) match {
            case (Const(f), Const(v)) => Const(i.mappend(f)(v))
        }
    }
}
