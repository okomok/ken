

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.annotation.tailrec


sealed abstract class List[+a] {
    @inline
    final def of[b >: a]: List[b] = this

    final def !!(n: Int): a = op_!!(this)(n)
    final def !::[b >: a](x: b): List[b] = op_!::[b](x)(this)

    final def filter(p: a => Boolean): List[a] = ken.filter(p)(this)
    final def withFilter(p: a => Boolean): List[a] = ken.filter(p)(this)

    final def toScalaList: scala.List[a] = this match {
        case Nil => scala.Nil
        case x :: xs => scala.::(x, xs.!.toScalaList)
    }
}


object Nil extends List[Nothing] {
    def ::[a](x: a): List[a] = new ::[a](x, Lazy(this))
}

case class ::[+a](head: a, tail: Lazy[List[a]]) extends List[a] {
    override def equals(that: Any): Boolean = that match {
        case that: List[_] => List.op_==(this)(that)
        case _ => false
    }
}


object !:: { // strict extractor
    def unapply[a](xs: List[a]): Option[(a, List[a])] = xs match {
        case Nil => None
        case x :: xs => Some(x, xs.!)
    }
}


object List extends Alternative[List] with MonadPlus[List] {
    implicit val theInstance = this

    @tailrec
    def op_==(xs: List[_])(ys: List[_]): Boolean = (xs, ys) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => false
        case (x :: xs, y :: ys) => {
            if (x == y) op_==(xs.!)(ys.!) else false
        }
    }

    private[ken] class OfName[a](xs: => List[a]) {
        def ::(x: a): List[a] = op_::(x)(xs)
        def :::(ys: List[a]): List[a] = op_++(ys)(xs)
    }
    implicit def ofName[a](xs: => List[a]): OfName[a] = new OfName(xs)

    private[this] type m[a] = List[a]
    // Alternative
    override def empty[a]: m[a] = Nil
    override def op_<|>[a](x: m[a])(y: => m[a]): m[a] = x ::: y
    // Monad
    override def `return`[a](x: => a): m[a] = x :: Nil
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = concat(map(y)(x))
    // MonadPlus
    override def mzero[a]: m[a] = Nil
    override def mplus[a](x: m[a])(y: => m[a]): m[a] = x ::: y

    implicit def monoidInstance[a]: Monoid[List[a]] = new Monoid[List[a]] {
        private[this] type m = List[a]
        override def mempty: m = Nil
        override def mappend(x: m)(y: => m): m = x ::: y
    }

    def from[a](that: List[a]): List[a] = that

    def apply[a](xs: a*): List[a] = from(xs)
    def unapplySeq[a](xs: List[a]): Option[Seq[a]] = Some(xs.toScalaList)

    implicit def fromIterable[a](xs: scala.Iterable[a]): List[a] = if (xs.isEmpty) Nil else (xs.head :: from(xs.tail))
}


object ZipList extends Applicative[List] {
    implicit val theInstance = this
    private[this] type f[a] = List[a]
    override def pure[a](x: => a): f[a] = repeat(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = zipWith[a => b, a, b](`@`)(x)(y)
}
