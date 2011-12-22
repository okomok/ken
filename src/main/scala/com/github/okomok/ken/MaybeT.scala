

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


final case class MaybeT[n[+_], +a](override val old: n[Maybe[a]]) extends NewtypeOf[n[Maybe[a]]]


object MaybeT extends MaybeTOp with MaybeTAs with MonadTrans[MaybeT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = MaybeT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[Maybe[a]]
    }

    // Overrides
    //
    // MonadTrans
    protected type t[n[+_], +a] = MaybeT[n, a]
    final case class StT[+a](override val old: Maybe[a]) extends NewtypeOf[Maybe[a]]
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = MaybeT {
        _N.liftM((a: a) => Maybe.`return`(a)) {
            f {
                new Run {
                    override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                        _U.liftM((x: Maybe[b]) => StT(x))(run(t))
                    }
                }
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = MaybeT {
        _N.liftM((St: StT[a]) => St.old)(nSt)
    }
}


private[ken] trait MaybeTOp {
    def run[n[+_], a](m: MaybeT[n, a]): n[Maybe[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[Maybe[a]] => u[Maybe[b]])(m: MaybeT[n, a]): MaybeT[u, b] = MaybeT { f(run(m)) }
}


private[ken] sealed trait MaybeTAs extends MonadTrans.Deriving0[MaybeT, MonadBase.type ^: MonadCont.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil] { this: MaybeT.type =>
    override protected def deriveMonad[n[+_]](_N: Monad[n]) = _asMonadPlus(_N)

    override protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override val selfMonad = deriveMonad[n](_N)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = MaybeT {
            _N.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                run( f( a => MaybeT { c(Just(a)) } ) )
            }
        }
    }

    implicit def _asMonadPlus[n[+_]](implicit _N: Monad[n]): MonadPlus[({type L[+a] = MaybeT[n, a]})#L] = new MonadPlus[({type L[+a] = MaybeT[n, a]})#L] {
        // Monad
        private type m[+a] = MaybeT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = MaybeT { _N.`return`(Just(a.!).up) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = MaybeT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Nothing => _N.`return`(Nothing.of[b])
                    case Just(r) => run(k(r))
                }
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { _N.`return`(Nothing) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = MaybeT {
            import _N.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Nothing => run(n)
                    case Just(r) => _N.`return`(Just(r))
                }
            }
        }
    }
}
