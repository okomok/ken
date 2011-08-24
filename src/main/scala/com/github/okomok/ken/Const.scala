

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class Const[a, +b](override val get: a) extends NewtypeOf[a]


object Const extends Kind.FunctionLike {
    sealed trait apply[z] extends Kind.AbstractNewtype1 {
        override type apply1[+a] = Const[z, a]
        override type oldtype1[+a] = z
    }

    implicit def _asNewtype1[z]: Newtype1[({type nt[+a] = Const[z, a]})#nt, ({type ot[+a] = z})#ot] = new Newtype1[({type nt[+a] = Const[z, a]})#nt, ({type ot[+a] = z})#ot] {
        private[this] type nt[+a] = Const[z, a]
        private[this] type ot[+a] = z
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = Const(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    }

    implicit def _asFunctor[z]: Functor[({type f[+a] = Const[z, a]})#f] = new Functor[({type f[+a] = Const[z, a]})#f] {
        private[this] type f[+a] = Const[z, a]
        override def fmap[a, b](a: a => b)(y: f[a]): f[b] = y match {
            case Const(v) => Const(v)
        }
    }

    implicit def _asApplicative[z](implicit i: Monoid[z]): Applicative[({type f[+a] = Const[z, a]})#f] = new Applicative[({type f[+a] = Const[z, a]})#f] {
        private[this] type f[+a] = Const[z, a]
        override def pure[a](a: Lazy[a]): f[a] = Const(i.mempty)
        override def op_<*>[a, b](a: f[a => b])(y: f[a]): f[b] = (a, y) match {
            case (Const(f), Const(v)) => Const(i.mappend(f)(v))
        }
    }
}
