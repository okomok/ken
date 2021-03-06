

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


sealed abstract class Maybe[+a] extends Up[Maybe[a]] {
    final def of[b >: a]: Maybe[b] = this
}

case object Nothing extends Maybe[Nothing]
final case class Just[+a](x: a) extends Maybe[a]


object Maybe extends MaybeAs with MonadPlus[Maybe] with Traversable[Maybe] with Extend[Maybe] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private type f[+a] = Maybe[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = {
        case Nothing => Nothing
        case Just(a) => Just(f(a))
    }
    // Monad
    private type m[+a] = Maybe[a]
    override def `return`[a](x: Lazy[a]): m[a] = Just(x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
        case Just(x) => k(x)
        case Nothing => Nothing
    }
    override def op_>>[b](m: m[_])(k: Lazy[m[b]]): m[b] = m match {
        case Just(_) => k
        case Nothing => Nothing
    }
    // MonadPlus
    override def mzero: m[Nothing] = Nothing
    override def mplus[a](xs: m[a])(ys: Lazy[m[a]]): m[a] = xs match {
        case Nothing => ys
        case _ => xs
    }
    // Foldable
    private type t[+a] = Maybe[a]
    override def foldr[a, b](f: a => Lazy[b] => b)(z: b)(t: t[a]): b = t match {
        case Nothing => z
        case Just(x) => f(x)(z)
    }
    override def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = t match {
        case Nothing => z
        case Just(x) => f(z)(x)
    }
    // Traversable
    override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = t match {
        case Nothing => i.pure(Nothing)
        case Just(x) => {
            import i.{<@>}
            (Just(_: b).up) <@> f(x)
        }
    }
    // Extend
    private type w[+a] = Maybe[a]
    override def duplicate[a](w: w[a]): w[w[a]] = w match {
        case Nothing => Nothing
        case j => Just(j)
    }

    // Operators
    //
    def maybe[a, b](n: b)(f: a => b)(m: Maybe[a]): b = m match {
        case Nothing => n
        case Just(x) => f(x)
    }

    def isJust[a](m: Maybe[a]): Bool = m match {
        case Nothing => False
        case _ => True
    }

    def isNothing[a](m: Maybe[a]): Bool = m match {
        case Nothing => True
        case _ => False
    }

    def fromJust[a](m: Maybe[a]): a = m match {
        case Nothing => error("Nothing")
        case Just(x) => x
    }

    def fromMaybe[a](d: Lazy[a])(x: Maybe[a]): a = x match {
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

    def orElse[a](x: Maybe[a])(y: Maybe[a]): Maybe[a] = x match {
        case Just(_) => x
        case Nothing => y
    }
}


private[ken] sealed trait MaybeAs { this: Maybe.type =>
    implicit def _asEq[a](implicit i: Eq[a]): Eq[Maybe[a]] = new Eq[Maybe[a]] {
        override val op_=== : op_=== = x => y => (x, y) match {
            case (Nothing, Nothing) => True
            case (Nothing, _) => False
            case (_, Nothing) => False
            case (Just(x), Just(y)) => i.op_===(x)(y)
        }
    }

    implicit def _asOrd[a](implicit i: Ord[a]): Ord[Maybe[a]] = new Ord[Maybe[a]] with EqProxy[Maybe[a]] {
        override val selfEq: selfEq = _asEq(i)
        override val compare: compare = x => y => (x, y) match {
            case (Nothing, Nothing) => EQ
            case (Nothing, _) => LT
            case (_, Nothing) => GT
            case (Just(x), Just(y)) => i.compare(x)(y)
        }
    }

    implicit def _asShow[a](implicit i: Show[a]): Show[Maybe[a]] = new Show[Maybe[a]] {
        override val showsPrec: showsPrec = _ => {
            case Nothing => Show.showString("Nothing")
            case Just(x) => Show.showString("Just(") `.` i.shows(x) `.` Show.showChar(')')
        }
    }

    implicit def _asMonoid[a](implicit i: Semigroup[a]): Monoid[Maybe[a]] = new Monoid[Maybe[a]] {
        override val mempty: mempty = Nothing
        override val mappend: mappend = m1 => m2 => (m1, m2.!) match {
            case (Nothing, m) => m
            case (m, Nothing) => m
            case (Just(m1), Just(m2)) => Just(i.op_<>:(m1)(m2))
        }
    }
}
