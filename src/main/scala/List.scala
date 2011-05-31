

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.annotation.tailrec
import Prelude._


sealed abstract class List[+A] {
    def ++[B >: A](that: => List[B]): List[B] = op_++[B](this)(that)
    def !!(n: Int): A = op_!!(this)(n)
}


object List extends Alternative[List] with MonadPlus[List] {
    implicit val theInstance = List

    case object Nil extends List[Nothing] {
        def ::[A](x: A): List[A] = new ::[A](x, this)
    }

    case class ::[+A](head: A, tail: Lazy[List[A]]) extends List[A] {
        override def equals(that: Any): Boolean = that match {
            case that: List[_] => op_==(this)(that)
            case _ => false
        }
    }

    object #:: { // strict extractor
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

    @tailrec
    def op_==(xs: List[_])(ys: List[_]): Boolean = (xs, ys) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => false
        case (x :: xs, y :: ys) => {
            if (x == y) op_==(xs())(ys()) else false
        }
    }

    private[this] type f[a] = List[a]
    // Applicative
    override def pure[a](x: => a): f[a] = x :: Nil
    // Alternative
    override def empty[a]: f[a] = Nil
    override def op_<|>[a](x: f[a])(y: f[a]): f[a] = x ++ y
    // Monad
    override def op_>>=[a, b](x: f[a])(y: a => f[b]): f[b] = concat(map(y)(x))
    // MonadPlus
    override def mzero[a]: f[a] = Nil
    override def mplus[a](x: f[a])(y: f[a]): f[a] = x ++ y

    implicit def monoidInstance[a]: Monoid[List[a]] = new Monoid[List[a]] {
        private[this] type m = List[a]
        override def mempty: m = Nil
        override def mappend(x: m)(y: m): m = x ++ y
    }
}


object ZipList extends Applicative[List] {
    private[this] type f[a] = List[a]
    // Applicative
    override def pure[a](x: => a): f[a] = repeat(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = zipWith[a => b, a, b](apply)(x)(y)
    implicit val instance = ZipList
}
