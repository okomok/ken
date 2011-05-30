

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import Prelude._


sealed abstract class List[+A] {
    def ++[B >: A](that: => List[B]): List[B] = op_++[B](this)(that)
    def !!(n: Int): A = op_!!(this)(n)
}


object List extends Alternative with MonadPlus {

    case object Nil extends List[Nothing] {
        def ::[A](x: A): List[A] = new ::[A](x, this)
    }

    case class ::[+A](head: A, tail: Lazy[List[A]]) extends List[A] {
        override def equals(that: Any): Boolean = that match {
            case that: List[_] => op_==(this)(that)
            case _ => false
        }
    }

    // strict extractor
    object #:: {
        def unapply[A](xs: List[A]): Option[(A, List[A])] = xs match {
            case Nil => None
            case x :: xs => Some(x, xs())
        }
    }

    private[ken] class OfName[A](xs: => List[A]) {
        def ::(x: A): List[A] = new List.::(x, xs)
        def :::(ys: List[A]): List[A] = ys ++ xs
    }
    implicit def ofName[A](xs: => List[A]): OfName[A] = new OfName(xs)

    // Functor
    override type f_[a] = List[a]
    // Applicative
    override def pure[a](x: => a): f_[a] = x :: Nil
    // Alternative
    override def empty[a]: f_[a] = Nil
    override def op_<|>[a](x: f_[a])(y: f_[a]): f_[a] = x ++ y
    // Monad
    override def op_>>=[a, b](x: f_[a])(y: a => f_[b]): f_[b] = concat(map(y)(x))
    // MonadPlus
    override def mzero[a]: f_[a] = Nil
    override def mplus[a](x: f_[a])(y: f_[a]): f_[a] = x ++ y

    class Of[a] extends Monoid {
        override type m_ = List[a]
        override def mempty: m_ = List.Nil
        override def mappend(x: m_)(y: m_): m_ = x ++ y
    }

    object Zip extends Applicative {
        // Functor
        override type f_[a] = List[a]
        // Applicative
        override def pure[a](x: => a): f_[a] = undefined
        override def op_<*>[a, b](x: f_[a => b])(y: f_[a]): f_[b] = undefined
    }
}

