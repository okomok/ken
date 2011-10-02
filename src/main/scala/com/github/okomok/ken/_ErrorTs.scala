

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


private[ken] final class _ErrorTs[n[+_]](override val inner: Monad[n]) extends MonadTs[n] {

    final case class _ErrorT[e, +a](override val get: n[Either[e, a]]) extends NewtypeOf[n[Either[e, a]]]

    object _ErrorT extends _ErrorTAs with Kind.FunctionLike {
        sealed trait apply[e] extends Kind.MonadTransControl {
            override type apply1[+a] = _ErrorT[e, a]
            override type oldtype1[+a] = n[Either[e, a]]
            override type innerMonad[+a] = n[a]
            override type baseResult[+a] = Either[e, a]
        }

        implicit def dependent[e, a](n: NewtypeOf[n[Either[e, a]]]): _ErrorT[e, a] = _ErrorT { n.run }

        def run[e, a](n: _ErrorT[e, a]): n[Either[e, a]] = n.run

        def map[e, e_, m[+_], a, b](f: n[Either[e, a]] => m[Either[e_, b]])(n: _ErrorT[e, a]): NewtypeOf[m[Either[e_, b]]] = NewtypeOf { f(run(n)) }
    }

    private[ken] sealed trait _ErrorTAs0 { this: _ErrorT.type =>
        implicit def _asNewtype1[e]: Newtype1[({type nt[+a] = _ErrorT[e, a]})#nt, ({type ot[+a] = n[Either[e, a]]})#ot] = new Newtype1[({type nt[+a] = _ErrorT[e, a]})#nt, ({type ot[+a] = n[Either[e, a]]})#ot] {
            private type nt[+a] = _ErrorT[e, a]
            private type ot[+a] = n[Either[e, a]]
            override def newOf[a](ot: Lazy[ot[a]]): nt[a] = _ErrorT(ot)
            override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
        }

        implicit def _asMonadTrans[e]: MonadTransControl[({type m[+a] = _ErrorT[e, a]})#m, n, ({type u[+a] = Either[e, a]})#u] = new MonadTransControl[({type m[+a] = _ErrorT[e, a]})#m, n, ({type u[+a] = Either[e, a]})#u] {
            // MonadTrans
            type m[+a] = _ErrorT[e, a]
            override def lift[a](n: n[a]): m[a] = _ErrorT {
                for { a <- n } yield Right(a)
            }
            // MonadTransControl
            type u[+a] = Either[e, a]
            override def liftControl[a](f: Run => n[a]): m[a] = _ErrorT {
                val em = Monad[Either.apply[e]]
                inner.liftM[a, Either[e, a]](em.`return`[a]) {
                    f {
                        new Run {
                            override def apply[o[+_], b](t: m[b])(implicit i: Monad[o]): n[o[u[b]]] = {
                                inner.liftM((x: Either[e, b]) => i.`return`(x))(run(t))
                            }
                        }
                    }
                }
            }
        }

        implicit def _asMonadFix[e](implicit i: MonadFix[n]): MonadFix[({type m[+a] = _ErrorT[e, a]})#m] = new MonadFix[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            override val selfMonad = _asMonadError[e]
            override def mfix[a](f: Lazy[a] => m[a]): m[a] = _ErrorT {
                def k(a: Lazy[Either[e, a]]) = run { f { a.! match {
                    case Right(r) => r
                    case _ => error("empty mfix argument")
                } } }
                i.mfix(k)
            }
        }

        implicit def _asMonadIO[e](implicit i: MonadIO[n]): MonadIO[({type m[+a] = _ErrorT[e, a]})#m] = new MonadIO[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            private val mt = _asMonadTrans[e]
            override val selfMonad = _asMonadError[e]
            override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
        }

        implicit def _asMonadCont[e](implicit i: MonadCont[n]): MonadCont[({type m[+a] = _ErrorT[e, a]})#m] = new MonadCont[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            override val selfMonad = _asMonadError[e]
            override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = _ErrorT {
                i.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                    run { f(a => _ErrorT { c(Right(a)) }) }
                }
            }
        }

        implicit def _asMonadReader[e, r](implicit i: MonadReader[r, n]): MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] = new MonadReader[r, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            override val selfMonad = _asMonadError[e]
            override def ask: m[r] = _asMonadTrans.lift(i.ask)
            override def local[a](f: r => r)(m: m[a]): m[a] = _ErrorT { i.local(f)(run(m)) }
        }

        implicit def _asMonadWriter[e, w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] = new MonadWriter[w, ({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            override val selfMonad = _asMonadError[e]
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
                            case Left(l) => inner.`return`(Left(l), id[w])
                            case Right((r, f)) => inner.`return`(Right(r), f)
                        }
                    } yield *
                }
            }
        }

        implicit def _asMonadPlus[e](implicit i: ErrorClass[e]): MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] = new MonadPlus[({type m[+a] = _ErrorT[e, a]})#m] with MonadProxy[({type m[+a] = _ErrorT[e, a]})#m] {
            private type m[+a] = _ErrorT[e, a]
            override val selfMonad = _asMonadError[e]
            override def mzero: m[Nothing] = _ErrorT { inner.`return`(Left(i.noMsg)) }
            override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(_) => run(n)
                        case Right(r) => inner.`return`(Right(r))
                    }
                } yield *
            }
        }

        @Annotation.compilerWorkaround("2.9.1") // `e` instead of `e_` confuses scalac.
        implicit def _asMonadControlIO[e_](implicit i: MonadControlIO[n]): MonadControlIO[({type m[+a] = _ErrorT[e_, a]})#m] = new MonadControlIO[({type m[+a] = _ErrorT[e_, a]})#m] with MonadIOProxy[({type m[+a] = _ErrorT[e_, a]})#m] {
            private type m[+a] = _ErrorT[e_, a]
            private val mt = _asMonadTrans[e_]
            override val selfMonadIO = _asMonadIO[e_](i)
            override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
                def dep[b](x: n[n[Either[e_, b]]]): n[m[b]] = for { n <- x } yield _ErrorT(n)
                mt.liftControl { run1 =>
                    i.liftControlIO { runInBase =>
                        f {
                            new RunInIO {
                                override def apply[b](t: m[b]): IO[m[b]] = IO.liftM((x: n[m[b]]) => join(mt.lift(x)))(runInBase(dep(run1[n, b](t))))
                            }
                        }
                    }
                }
            }
        }
    }

    private[ken] sealed trait _ErrorTAs extends _ErrorTAs0 { this: _ErrorT.type =>
        implicit def _asMonadError[e]: MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m] = new MonadError[e, ({type m[+a] = _ErrorT[e, a]})#m] {
            // Functor
            private type f[+a] = _ErrorT[e, a]
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
            private type m[+a] = f[a]
            override def `return`[a](a: Lazy[a]): m[a] = _ErrorT { inner.`return`(Right(a.!)) }
            override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = _ErrorT {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(l) => inner.`return`(Left(l))
                        case Right(r) => run(k(r))
                    }
                } yield *
            }
            // MonadError
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
    }
}
