

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


final case class ErrorT[e, n[+_], +a](override val old: n[Either[e, a]]) extends NewtypeOf[n[Either[e, a]]]


object ErrorT extends ErrorTOp with ErrorTAs with Kind.FunctionLike {
    trait apply[e] extends apply1[e]
    trait apply1[e] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = ErrorT[e, n, a]
    }

    trait apply2[e, n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = ErrorT[e, n, a]
        override type oldtype1[+a] = n[Either[e, a]]
    }
}


private[ken] trait ErrorTOp {
    def run[e, n[+_], a](m: ErrorT[e, n, a]): n[Either[e, a]] = m.run

    def map[e, e_, n[+_], u[+_], a, b](f: n[Either[e, a]] => u[Either[e_, b]])(m: ErrorT[e, n, a]): ErrorT[e_, u, b] = ErrorT { f(run(m)) }
}


private[ken] sealed trait ErrorTAs0 { this: ErrorT.type =>
/*
    // No longer works...
    @Annotation.typeAliasWorkaround
    implicit def _asNewtype1[e, n[+_]]: Newtype1[apply2[e, n]#apply1, apply2[e, n]#oldtype1] = new Newtype1[apply2[e, n]#apply1, apply2[e, n]#oldtype1] {
        // private type nt[+a] = ErrorT[e, n, a]
        // private type ot[+a] = n[Either[e, a]]
        override def newOf[a](ot: Lazy[n[Either[e, a]]]): ErrorT[e, n, a] = ErrorT(ot)
        override def oldOf[a](nt: Lazy[ErrorT[e, n, a]]): n[Either[e, a]] = nt.run
    }
*/
    implicit def _asMonadTrans[e, n[+_]]: MonadTransControl[apply1[e]#monadTrans] = new MonadTransControl[apply1[e]#monadTrans] {
        // MonadTrans
        private type t[n[+_], +a] = ErrorT[e, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = ErrorT {
            import i.`for`
            for { a <- n } yield Right(a)
        }
        // MonadTransControl
        override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = ErrorT {
            val em = Monad[Either.apply[e]]
            i.liftM((x: a) => em.`return`(x)) {
                f {
                    new Run {
                        override def apply[n_[+_], o[+_], b](t: t[n_, b], * : TypeC1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                            ri.liftM((x: Either[e, b]) => ErrorT(rj.`return`(x)))(run(t))
                        }
                    }
                }
            }
        }
    }

    implicit def _asMonadFix[e, n[+_]](implicit i: MonadFix[n]): MonadFix[apply2[e, n]#apply1] = new MonadFix[apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        override val selfMonad = _asMonadError[e, n]
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = ErrorT {
            def k(a: Lazy[Either[e, a]]) = run { f { a.! match {
                case Right(r) => r
                case _ => error("empty mfix argument")
            } } }
            i.mfix(k)
        }
    }

    implicit def _asMonadCont[e, n[+_]](implicit i: MonadCont[n]): MonadCont[apply2[e, n]#apply1] = new MonadCont[apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        override val selfMonad = _asMonadError[e, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ErrorT {
            i.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                run { f(a => ErrorT { c(Right(a)) }) }
            }
        }
    }

    implicit def _asMonadReader[e, n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply2[e, n]#apply1] = new MonadReader[r, apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        override val selfMonad = _asMonadError[e, n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = ErrorT { i.local(f)(run(m)) }
    }

    implicit def _asMonadWriter[e, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, apply2[e, n]#apply1] = new MonadWriter[w, apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        override val selfMonad = _asMonadError[e, n]
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
        override def listen[a](m: m[a]): m[(a, w)] = ErrorT {
            import i.`for`
            for {
                (a, w) <- i.listen(run(m))
                * <- a match {
                    case Left(l) => i.`return`(Left(l))
                    case Right(r) => i.`return`(Right(r, w))
                }
            } yield *
        }
        override def pass[a](m: m[(a, w => w)]): m[a] = ErrorT {
            import i.`for`
            i.pass {
                for {
                    a <- run(m)
                    * <- a match {
                        case Left(l) => i.`return`(Left(l), id[w])
                        case Right((r, f)) => i.`return`(Right(r), f)
                    }
                } yield *
            }
        }
    }

    implicit def _asMonadPlus[e, n[+_]](implicit i: Monad[n], j: ErrorClass[e]): MonadPlus[apply2[e, n]#apply1] = new MonadPlus[apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        override val selfMonad = _asMonadError[e, n]
        override def mzero: m[Nothing] = ErrorT { i.`return`(Left(j.noMsg)) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = ErrorT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Left(_) => run(n)
                    case Right(r) => i.`return`(Right(r))
                }
            } yield *
        }
    }

    implicit def _asMonadIO[e, n[+_]](implicit i: MonadIO[n]): MonadIO[apply2[e, n]#apply1] = new MonadIO[apply2[e, n]#apply1] with MonadProxy[apply2[e, n]#apply1] {
        private type m[+a] = ErrorT[e, n, a]
        private val mt = _asMonadTrans[e, n]
        override val selfMonad = _asMonadError[e, n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait ErrorTAs1 extends ErrorTAs0 { this: ErrorT.type =>
    @Annotation.compilerWorkaround("2.9.1") // `e` instead of `e_` confuses scalac.
    implicit def _asMonadControlIO[e_, n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[apply2[e_, n]#apply1] = new MonadControlIO[apply2[e_, n]#apply1] with MonadIOProxy[apply2[e_, n]#apply1] {
        private type m[+a] = ErrorT[e_, n, a]
        private val mt = _asMonadTrans[e_, n]
        override val selfMonadIO = _asMonadIO[e_, n]
        override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
            mt.liftControl { run1 =>
                i.liftControlIO { runInBase =>
                    f {
                        new RunInIO {
                            override def apply[b](t: m[b]): IO[m[b]] = IO.liftM((x: n[m[b]]) => join(mt.lift(x)))(runInBase(run1(t)))
                        }
                    }
                }
            }
        }
    }
}

private[ken] sealed trait ErrorTAs extends ErrorTAs1 { this: ErrorT.type =>
    implicit def _asMonadError[e, n[+_]](implicit i: Monad[n]): MonadError[e, apply2[e, n]#apply1] = new MonadError[e, apply2[e, n]#apply1] {
        // Functor
        private type f[+a] = ErrorT[e, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ErrorT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Left(l) => i.`return`(Left(l))
                    case Right(r) => i.`return`(Right(f(r)))
                }
            } yield *
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ErrorT { i.`return`(Right(a.!)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ErrorT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Left(l) => i.`return`(Left(l))
                    case Right(r) => run(k(r))
                }
            } yield *
        }
        // MonadError
        override def throwError[a](l: e): m[a] = ErrorT { i.`return`(Left(l)) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = ErrorT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Left(l) => run(h(l))
                    case Right(r) => i.`return`(Right(r))
                }
            } yield *
        }
    }
}
