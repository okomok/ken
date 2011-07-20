

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Either[+a, +b] extends Up[Either[a, b]] {
    @inline
    final def of[a_ >: a, b_ >: b]: Either[a_, b_] = this
}

final case class Left[+a](x: a) extends Either[a, Nothing]
final case class Right[+b](y: b) extends Either[Nothing, b]


trait EitherLowPriorityImplicits { this: Either.type =>
    implicit def monad[e]: MonadFix[({type m[+a] = Either[e, a]})#m] = new MonadFix[({type m[+a] = Either[e, a]})#m] {
        // Functor
        private[this] type f[+a] = Either[e, a]
        override def fmap[a, b](f: a => b)(e: f[a]): f[b] = e match {
            case Left(l) => Left(l)
            case Right(r) => Right(f(r))
        }
        // Monad
        private[this] type m[+a] = f[a]
        override def `return`[a](x: a): m[a] = Right(x)
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
            case Left(l) => Left(l)
            case Right(r) => k(r)
        }
        // MonadFix
        override def mfix[a](f: (=> a) => m[a]): m[a] = {
            lazy val a: m[a] = f { a match {
                case Right(r) => r
                case _ => error("empty mfix argument")
            } }
            a
        }
    }
}


object Either extends EitherLowPriorityImplicits {
    def either[a, b, c](f: a => c)(g: b => c)(e: Either[a, b]): c = e match {
        case Left(x) => f(x)
        case Right(y) => g(y)
    }

    def lefts[a, b](x: List[Either[a, b]]): List[a] = for { Left(a) <- x } yield a
    def rights[a, b](x: List[Either[a, b]]): List[b] = for { Right(a) <- x } yield a

    def partitionEithers[a, b](x: List[Either[a, b]]): (List[a], List[b]) = {
        def left(_a: a)(lr: => (List[a], List[b])): (List[a], List[b]) = lr match { case (l, r) => (_a :: l, r) }
        def right(_a: b)(lr: => (List[a], List[b])): (List[a], List[b]) = lr match { case (l, r) => (l, _a :: r) }
        List.foldr[Either[a, b], (List[a], List[b])](either(left)(right))((Nil, Nil))(x)
    }

    /*
    implicit def monadError[e](implicit i: ErrorClass[e]): MonadPlus[({type m[+a] = Either[e, a]})#m] with MonadError[e, ({type m[+a] = Either[e, a]})#m]
        = new MonadPlus[({type m[+a] = Either[e, a]})#m] with MonadError[e, ({type m[+a] = Either[e, a]})#m] with MonadProxy[({type m[+a] = Either[e, a]})#m]
    {
        override val self = monad[e]
        // MonadPlus
        private[this] type m[+a] = Either[e, a]
        override def mzero: m[Nothing] = Left(i.noMsg)
        override def mplus[a](m: m[a])(n: => m[a]): m[a] = m match {
            case Left(_) => n
            case _ => m
        }
        // MonadError
        override def errorClass: ErrorClass[e] = i
        override def throwError[a](e: e): m[a] = Left(e)
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = m match {
            case Left(l) => h(l)
            case Right(r) => Right(r)
        }
    }
    */
}
