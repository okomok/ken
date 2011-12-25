

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

    trait apply2[e, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = ErrorT[e, n#apply1, a]
        override type oldtype1[+a] = n#apply1[Either[e, a]]
    }
}


private[ken] trait ErrorTOp {
    def run[e, n[+_], a](m: ErrorT[e, n, a]): n[Either[e, a]] = m.run

    def map[e, e_, n[+_], u[+_], a, b](f: n[Either[e, a]] => u[Either[e_, b]])(m: ErrorT[e, n, a]): ErrorT[e_, u, b] = ErrorT { f(run(m)) }
}


private[ken] sealed trait ErrorTAs0 extends MonadTransControl.Deriving1[ErrorT, ErrorClass, Monad.type ^: MonadBaseControl.type ^: MonadCont.type ^: MonadIO.type ^: MonadReader.type ^: MonadState.type ^: MonadWriter.type ^: Kind.Nil] { this: ErrorT.type =>
    private type t1[z, n[+_], +a] = ErrorT[z, n, a]
    private type c[z] = ErrorClass[z]

    override protected def asMonadTransControl[z](_C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] {
        private type t[n[+_], +a] = t1[z, n, a]
        private type e = z
        override type StT[+a] = Either[e, a]
        override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = ErrorT {
            val _Me = Monad[Either.apply[e]]
            _N.liftM((a: a) => _Me.`return`(a)) {
                f {
                    new Run {
                        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = run(t)
                    }
                }
            }
        }
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = ErrorT(nSt)
    }

    override protected def deriveMonad[z, n[+_]](_N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L] = _asMonadError(_N)

    override protected def deriveMonadFix[z, n[+_]](_N: MonadFix[n], _C: c[z]): MonadFix[({type L[+a] = t1[z, n, a]})#L] = new MonadFix[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type e = z
        override val selfMonad: selfMonad = deriveMonad(_N, _C)
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = ErrorT {
            def k(a: Lazy[Either[e, a]]) = run { f { a.! match {
                case Right(r) => r
                case _ => error("empty mfix argument")
            } } }
            _N.mfix(k)
        }
    }

    override protected def deriveMonadCont[z, n[+_]](_N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = new MonadCont[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type e = z
        override val selfMonad: selfMonad = deriveMonad(_N, _C)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ErrorT {
            _N.callCC { (c: Either[e, a] => n[Either[e, b]]) =>
                run { f(a => ErrorT { c(Right(a)) }) }
            }
        }
    }

    override protected def deriveMonadWriter[z, n[+_], w](_N: MonadWriter[w, n], _C: c[z]): MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] = new MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type e = z
        override val selfMonad: selfMonad = deriveMonad(_N, _C)
        override def monoid: Monoid[w] = _N.monoid
        override val tell: w => m[Unit] = x => asMonadTransControl(_C).lift(_N.tell(x))(_N)
        override def listen[a](m: m[a]): m[(a, w)] = ErrorT {
            import _N.`for`
            for {
                (a, w) <- _N.listen(run(m))
            } {
                a match {
                    case Left(l) => _N.`return`(Left(l))
                    case Right(r) => _N.`return`(Right(r, w))
                }
            }
        }
        override def pass[a](m: m[(a, w => w)]): m[a] = ErrorT {
            import _N.`for`
            _N.pass {
                for {
                    a <- run(m)
                } {
                    a match {
                        case Left(l) => _N.`return`(Left(l), id[w])
                        case Right((r, f)) => _N.`return`(Right(r), f)
                    }
                }
            }
        }
    }

    implicit def _asMonadPlus[z, n[+_]](implicit _N: Monad[n], _C: c[z]): MonadPlus[({type L[+a] = t1[z, n, a]})#L] = new MonadPlus[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type e = z
        override val selfMonad: selfMonad = deriveMonad(_N, _C)
        override val mzero: m[Nothing] = ErrorT { _N.`return`(Left(_C.noMsg)) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = ErrorT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Left(_) => run(n)
                    case Right(r) => _N.`return`(Right(r))
                }
            }
        }
    }
}

private[ken] sealed trait ErrorTAs extends ErrorTAs0 { this: ErrorT.type =>
    implicit def _asMonadError[e, n[+_]](implicit _N: Monad[n]): MonadError[e, ({type L[+a] = ErrorT[e, n, a]})#L] = new MonadError[e, ({type L[+a] = ErrorT[e, n, a]})#L] {
        // Functor
        private type f[+a] = ErrorT[e, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ErrorT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Left(l) => _N.`return`(Left(l))
                    case Right(r) => _N.`return`(Right(f(r)))
                }
            }
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ErrorT { _N.`return`(Right(a.!)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ErrorT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Left(l) => _N.`return`(Left(l))
                    case Right(r) => run(k(r))
                }
            }
        }
        // MonadError
        override val throwError: throwError = l => ErrorT { _N.`return`(Left(l)) }
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = ErrorT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Left(l) => run(h(l))
                    case Right(r) => _N.`return`(Right(r))
                }
            }
        }
    }
}
