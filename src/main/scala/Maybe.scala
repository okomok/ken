

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Maybe[+a] extends Up[Maybe[a]] {
    @inline
    final def of[b >: a]: Maybe[b] = this
}


object Nothing extends Maybe[scala.Nothing]

case class Just[+a](x: a) extends Maybe[a]


object Maybe {
    def maybe[a, b](n: b)(f: a => b)(m: Maybe[a]): b = m match {
        case Nothing => n
        case Just(x) => f(x)
    }

    def isJust[a](m: Maybe[a]): Boolean = m match {
        case Nothing => false
        case _ => true
    }

    def isNothing[a](m: Maybe[a]): Boolean = m match {
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
        import Monad.`for`
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

    implicit object theInstance extends MonadPlus[Maybe] {
        private[this] type m[a] = Maybe[a]
        // Monad
        override def `return`[a](x: a): m[a] = Just(x)
        override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = x match {
            case Just(x) => y(x)
            case Nothing => Nothing
        }
        // MonadPlus
        override def mzero[a]: m[a] = Nothing
        override def mplus[a](x: m[a])(y: => m[a]): m[a] = (x, y) match {
            case (Nothing, p) => p
            case (Just(p), _) => Just(p)
        }
    }
}
