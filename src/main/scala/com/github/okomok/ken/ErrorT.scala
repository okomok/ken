

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


private[ken] final class _ErrorTs[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerFor[a](x: n[a]): inner.For[a] = inner.`for`(x)

    sealed abstract class _ErrorT[e, +a] extends Strong[n[Either[e, a]]]

    object _ErrorT extends Kind.Function1nv with Instance {
        sealed trait apply[e] extends Kind.MonadTrans {
            override type apply[+a] = _ErrorT[e, a]
            override type inner[+a] = n[a]
            override type weak[+a] = n[Either[e, a]]
        }

        def apply[e, a](rep: n[Either[e, a]]): _ErrorT[e, a] = new _ErrorT[e, a] {
            override def get: n[Either[e, a]] = rep
        }

        implicit def from[e, a](n: Strong[n[Either[e, a]]]): _ErrorT[e, a] = _ErrorT { n.run }

        def run[e, a](n: _ErrorT[e, a]): n[Either[e, a]] = n.run

        def map[e, e_, m[+_], a, b](f: n[Either[e, a]] => m[Either[e_, b]])(n: _ErrorT[e, a]): Strong[m[Either[e_, b]]] = Strong { f(run(n)) }
    }

    private[ken] trait Instance0 { this: _ErrorT.type =>
        implicit def weak[e]: Imply1[({type p[+a] = _ErrorT[e, a]})#p, ({type d[+a] = n[Either[e, a]]})#d] =
            new Imply1[({type p[+a] = _ErrorT[e, a]})#p, ({type d[+a] = n[Either[e, a]]})#d]
        {
            private[this] type p[+a] = _ErrorT[e, a]
            private[this] type d[+a] = n[Either[e, a]]
            override def imply[a](p: p[a]): d[a] = run(p)
            override def unimply[a](d: => d[a]): p[a] = _ErrorT(d)
        }

        implicit def _monad[e](implicit i: ErrorClass[e]): MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] with MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m]
        {
            // Functor
            private[this] type f[+a] = _ErrorT[e, a]
            override def fmap[a, b](f: a => b)(m: f[a]): f[b] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => inner.`return`(Right(f(r)))
                    }
                } yield *
            }
            // Monad
            private[this] type m[+a] = f[a]
            override def `return`[a](a: => a): m[a] = _ErrorT { inner.`return`(Right(a)) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => run(k(r))
                    }
                } yield *
            }
            // MonadPlus
            override def mzero: m[Nothing] = _ErrorT { inner.`return`(Left(i.noMsg)) }
            override def mplus[a](m: m[a])(n: => m[a]): m[a] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(_) => run(n)
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
            // MonadError
            override def errorClass: ErrorClass[e] = i
            override def throwError[a](l: e): m[a] = _ErrorT { inner.`return`(Left(l)) }
            override def catchError[a](m: m[a])(h: e => m[a]): m[a] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(l) => run(h(l))
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
        }

        implicit def _asMonadTrans[e]: MonadTrans[n, ({type m[+a] = _ErrorT[e, a]})#m] = new MonadTrans[n, ({type m[+a] = _ErrorT[e, a]})#m] {
            private[this] type m[+a] = _ErrorT[e, a]
            override def lift[a](n: n[a]): m[a] = _ErrorT {
                for { a <- n } yield Right(a)
            }
        }
    }

    private[ken] trait Instance1 extends Instance0 { this: _ErrorT.type =>
        implicit def _asMonadFix[e](implicit i: MonadFix[n], j: ErrorClass[e]): MonadFix[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadFix[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = _monad[e]
            override def mfix[a](f: (=> a) => m[a]): m[a] = _ErrorT {
                def k(a: => Either[e, a]) = run { f { a match {
                    case Right(r) => r
                    case _ => error("empty mfix argument")
                } } }
                i.mfix(k)
            }
        }
    }

    private[ken] trait Instance2 extends Instance1 { this: _ErrorT.type =>
        implicit def _asMonadIO[e](implicit i: MonadIO[n], j: ErrorClass[e]): MonadIO[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadIO[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = _monad[e]
            override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
        }
    }

    private[ken] trait Instance3 extends Instance2 { this: _ErrorT.type =>
        implicit def _asMonadCont[e](implicit i: MonadCont[n], j: ErrorClass[e]): MonadCont[({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadCont[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = _monad[e]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ErrorT {
                i.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                    run { f(a => _ErrorT { c(Right(a)) }) }
                }
            }
        }
    }

    private[ken] trait Instance4 extends Instance3 { this: _ErrorT.type =>
        implicit def _asMonadReader[e, r](implicit i: MonadReader[r, n], j: ErrorClass[e]): MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = _monad[e]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _ErrorT { i.local(f)(run(m)) }
        }
    }

    private[ken] trait Instance5 extends Instance4 { this: _ErrorT.type =>
        implicit def _asMonadWriter[e, w](implicit i: MonadWriter[w, n], j: ErrorClass[e]): MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] =
            new MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m]
        {
            private[this] type m[+a] = _ErrorT[e, a]
            override val self = _monad[e]
            override def monoid: Monoid[w] = i.monoid
            override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
            override def listen[a](m: m[a]): m[(a, w)] = _ErrorT {
                for {
                    (a, w) <- i.listen(run(m))
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => inner.`return`(Right(r, w))
                    }
                } yield *
            }
            override def pass[a](m: m[(a, w => w)]): m[a] = _ErrorT {
                i.pass {
                    for {
                        a <- run(m)
                        * <- a match {
                            case Left(l) => inner.`return`(Left(l), id[w]_)
                            case Right((r, f)) => inner.`return`(Right(r), f)
                        }
                    } yield *
                }
            }
        }
    }

    private[ken] trait Instance extends Instance5 { this: _ErrorT.type =>
    }
}
