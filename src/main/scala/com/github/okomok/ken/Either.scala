

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


sealed abstract class Either[+a, +b] extends Up[Either[a, b]] {
    final def of[a_ >: a, b_ >: b]: Either[a_, b_] = this
}

final case class Left[+a](x: a) extends Either[a, Nothing]
final case class Right[+b](y: b) extends Either[Nothing, b]


object Either extends EitherAs with Kind.qcurry2[Either] {
    def either[a, b, c](f: a => c)(g: b => c)(e: Either[a, b]): c = e match {
        case Left(x) => f(x)
        case Right(y) => g(y)
    }

    def lefts[a, b](x: List[Either[a, b]]): List[a] = for { Left(a) <- x } yield a
    def rights[a, b](x: List[Either[a, b]]): List[b] = for { Right(a) <- x } yield a

    def partitionEithers[a, b](x: List[Either[a, b]]): (List[a], List[b]) = {
        def left(_a: a)(lr: Lazy[(List[a], List[b])]): (List[a], List[b]) = lr.! match { case (l, r) => (_a :: l, r) }
        def right(_a: b)(lr: Lazy[(List[a], List[b])]): (List[a], List[b]) = lr.! match { case (l, r) => (l, _a :: r) }
        List.foldr[Either[a, b], (List[a], List[b])](either(left)(right))((Nil, Nil))(x)
    }
}


private[ken] sealed trait EitherAs { this: Either.type =>
    implicit def _asEq[a, b](implicit i: Eq[a], j: Eq[b]): Eq[Either[a, b]] = new Eq[Either[a, b]] {
        override val op_=== : op_=== = x => y => (x, y) match {
            case (Left(x), Left(y)) => i.op_===(x)(y)
            case (Left(x), _) => False
            case (_, Left(x)) => False
            case (Right(x), Right(y)) => j.op_===(x)(y)
        }
    }

    implicit def _asOrd[a, b](implicit i: Ord[a], j: Ord[b]): Ord[Either[a, b]] = new Ord[Either[a, b]] with EqProxy[Either[a, b]] {
        override val selfEq = _asEq(i, j)
        override val compare: compare = x => y => (x, y) match {
            case (Left(x), Left(y)) => i.compare(x)(y)
            case (Left(x), _) => LT
            case (_, Left(x)) => GT
            case (Right(x), Right(y)) => j.compare(x)(y)
        }
    }

    implicit def _asShow[a, b](implicit i: Show[a], j: Show[b]): Show[Either[a, b]] = new Show[Either[a, b]] {
        override val showsPrec: showsPrec = _ => {
            case Left(x) => Show.showString("Left(") `.` i.shows(x) `.` Show.showChar(')')
            case Right(x) => Show.showString("Right(") `.` j.shows(x) `.` Show.showChar(')')
        }
    }

    implicit def _asSemigroup[a, b]: Semigroup[Either[a, b]] = new Semigroup[Either[a, b]] {
        override val op_<>: : op_<>: = a => b => (a, b.!) match {
            case (Left(_), b) => b
            case (a, _) => a
        }
    }

    implicit def _asMonadFix[e]: MonadFix[({type m[+a] = Either[e, a]})#m] = new MonadFix[({type m[+a] = Either[e, a]})#m] {
        // Functor
        private type f[+a] = Either[e, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = {
            case Left(l) => Left(l)
            case Right(r) => Right(f(r))
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](x: Lazy[a]): m[a] = Right(x.!)
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
            case Left(l) => Left(l)
            case Right(r) => k(r)
        }
        // MonadFix
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = {
            lazy val a: m[a] = f { a match {
                case Right(r) => r
                case _ => error("empty mfix argument")
            } }
            a
        }
    }
}
