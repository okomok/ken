

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Either[+a, +b] {
    @inline
    final def of[a_ >: a, b_ >: b]: Either[a_, b_] = this
    @inline
    final def asEither: Either[a, b] = this
}

case class Left[+a, +b](x: a) extends Either[a, b]
case class Right[+a, +b](y: b) extends Either[a, b]


object Either {
    def either[a, b, c](f: a => c)(g: b => c)(e: Either[a, b]): c = e match {
        case Left(x) => f(x)
        case Right(y) => g(y)
    }

    import Monad.`for`

    def lefts[a, b](x: List[Either[a, b]]): List[a] = for { Left(a) <- x } yield a
    def rights[a, b](x: List[Either[a, b]]): List[b] = for { Right(a) <- x } yield a

    def partitionEithers[a, b](x: List[Either[a, b]]): (List[a], List[b]) = {
        def left(_a: a)(lr: => (List[a], List[b])): (List[a], List[b]) = lr match { case (l, r) => (_a :: l, r) }
        def right(_a: b)(lr: => (List[a], List[b])): (List[a], List[b]) = lr match { case (l, r) => (l, _a :: r) }
        List.foldr[Either[a, b], (List[a], List[b])](either(left)(right))((Nil, Nil))(x)
    }

    implicit def functorInstance[A]: Functor[({type f[x] = Either[A, x]})#f] = new Functor[({type f[x] = Either[A, x]})#f] {
        private[this] type f[x] = Either[A, x]
        override def fmap[a, b](f: a => b)(e: f[a]): f[b] = e match {
            case Left(x) => Left(x)
            case Right(y) => Right(f(y))
        }
    }
}
