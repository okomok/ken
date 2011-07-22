

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


final class _ErrorTs[n[+_]](val inner: Monad[n]) {

    type _ErrorT[e, +a] = n[Either[e, a]]

    trait LowPriorityImplicits { this: _ErrorT.type =>
        implicit def monad[e](implicit i: ErrorClass[e]): MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m] with inner.Trans[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m] with inner.Trans[({type m[+a] = _ErrorT[e, a]})#m]
        {
            // Functor
            private[this] type f[+a] = _ErrorT[e, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = {
                import inner.`for` // ambiguity buster
                for { a <- m } yield (a match {
                    case Left(l) => Left(l)
                    case Right(r) => Right(f(r))
                })
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: a): m[a] = inner.`return`(Right(a))
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => k(r)
                    }
                } yield *
            }
            // MonadPlus
            override def mzero: m[Nothing] = inner.`return`(Left(i.noMsg))
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(_) => n
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
            // MonadError
            override def errorClass: ErrorClass[e] = i
            override def throwError[a](l: e): m[a] = inner.`return`(Left(l))
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = {
                import inner.`for`
                for {
                    a <- m
                    * <- a match {
                        case Left(l) => h(l)
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
            // Trans
            override def lift[a](n: n[a]): m[a] = {
                import inner.`for`
                for { a <- n } yield Right(a)
            }
        }
    }

    object _ErrorT extends LowPriorityImplicits {
        implicit def monadFix[e](implicit i: MonadFix[n], j: ErrorClass[e]): MonadFix[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadFix[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = monad[e]
            override def mfix[a](f: (=> a) => m[a]): m[a] = {
                def k(a: => Either[e, a]) = f { a match {
                    case Right(r) => r
                    case _ => error("empty mfix argument")
                } }
                i.mfix(k)
            }
        }

        implicit def monadIO[e](implicit i: MonadIO[n], j: ErrorClass[e]): MonadIO[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadIO[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = monad[e]
            override def liftIO[a](io: IO[a]): m[a] = self.lift(i.liftIO(io))
        }

        implicit def monadCont[e](implicit i: MonadCont[n], j: ErrorClass[e]): MonadCont[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadCont[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = monad[e]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = {
                i.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                    f(a => c(Right(a)))
                }
            }
        }

        implicit def monadReader[e, r](implicit i: MonadReader[r, n], j: ErrorClass[e]): MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = monad[e]
            override def ask: m[r] = self.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = i.local(f)(m)
        }

        implicit def monadWriter[e, w](implicit i: MonadWriter[w, n], j: ErrorClass[e]): MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = monad[e]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = self.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = {
                import inner.`for`
                for { (a, w) <- i.listen(m) } yield (a match {
                    case Left(l) => Left(l)
                    case Right(r) => Right(r, w)
                })
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = {
                import inner.`for`
                i.pass {
                    for {
                        a <- m
                    } yield (a match {
                        case Left(l) => (Left(l), id[w]_)
                        case Right((r, f)) => (Right(r), f)
                    })
                }
            }
        }
    }
}
