

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Maybe[+a] extends Up[Maybe[a]] {
    @inline
    final def of[b >: a]: Maybe[b] = this
}

case object Nothing extends Maybe[Nothing]
case class Just[+a](x: a) extends Maybe[a]


object Maybe extends MonadPlus[Maybe] {
// Overrides
    // Functor
    private[this] type f[+a] = Maybe[a]
    override def fmap[a, b](f: a => b)(x: f[a]): f[b] = x match {
        case Nothing => Nothing
        case Just(a) => Just(f(a))
    }
    // Monad
    private[this] type m[+a] = f[a]
    override def `return`[a](x: => a): m[a] = Just(x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
        case Just(x) => k(x)
        case Nothing => Nothing
    }
    override def op_>>[b](m: m[_])(k: => m[b]): m[b] = m match {
        case Just(_) => k
        case Nothing => Nothing
    }
    // MonadPlus
    override def mzero: m[Nothing] = Nothing
    override def mplus[a](xs: m[a])(ys: => m[a]): m[a] = xs match {
        case Nothing => ys
        case _ => xs
    }

// Instances
    implicit val monad: MonadPlus[Maybe] = this

// Operators
    def just[a](x: a): Maybe[a] = Just(x)

    def maybe[a, b](n: b)(f: a => b)(m: Maybe[a]): b = m match {
        case Nothing => n
        case Just(x) => f(x)
    }

    def isJust[a](m: Maybe[a]): Bool = m match {
        case Nothing => false
        case _ => true
    }

    def isNothing[a](m: Maybe[a]): Bool = m match {
        case Nothing => true
        case _ => false
    }

    def fromJust[a](m: Maybe[a]): a = m match {
        case Nothing => error("Nothing")
        case Just(x) => x
    }

    def fromMaybe[a](d: a)(x: Maybe[a]): a = x match {
        case Nothing => d
        case Just(v) => v
    }

    def maybeToList[a](m: Maybe[a]): List[a] = m match {
        case Nothing => Nil
        case Just(x) => List(x)
    }

    def listToMaybe[a](xs: List[a]): Maybe[a] = xs match {
        case Nil => Nothing
        case a :: _ => Just(a)
    }

    def catMaybes[a](ls: List[Maybe[a]]): List[a] = {
        for { Just(x) <- ls } yield x
    }

    def mapMaybe[a, b](f: a => Maybe[b])(xs: List[a]): List[b] = xs match {
        case Nil => Nil
        case x :: xs => {
            lazy val rs: List[b] = mapMaybe(f)(xs.!)
            f(x) match {
                case Nothing => rs
                case Just(r) => r :: rs
            }
        }
    }
}
