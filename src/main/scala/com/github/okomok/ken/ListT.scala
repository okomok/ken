

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


final case class ListT[n[+_], +a](override val old: n[List[a]]) extends NewtypeOf[n[List[a]]]


object ListT extends ListTOp with ListTAs with MonadTransControl[ListT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = ListT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[List[a]]
    }

    // Overrides
    //
    // MonadTransControl
    protected type t[n[+_], +a] = ListT[n, a]
    final case class StT[+a](override val old: List[a]) extends NewtypeOf[List[a]]
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = ListT {
        _N.liftM((a: a) => List.`return`(a)) {
            f {
                new Run {
                    override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                        _U.liftM((x: List[b]) => StT(x))(run(t))
                    }
                }
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = ListT {
        _N.liftM((St: StT[a]) => St.old)(nSt)
    }
}


private[ken] trait ListTOp {
    def run[n[+_], a](m: ListT[n, a]): n[List[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[List[a]] => u[List[b]])(m: ListT[n, a]): ListT[u, b] = ListT { f(run(m)) }
}


private[ken] sealed trait ListTAs extends MonadTransControl.Deriving0[ListT, MonadBaseControl.type ^: MonadCont.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil] { this: ListT.type =>
    override protected def deriveMonad[n[+_]](_N: Monad[n]) = _asMonadPlus(_N)

    override protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = deriveMonad(_N)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ListT {
            _N.callCC { (c: List[a] => n[List[b]]) =>
                run( f( a => ListT { c(List(a)) } ) )
            }
        }
    }

    implicit def _asMonadPlus[n[+_]](implicit _N: Monad[n]): MonadPlus[({type L[+a] = ListT[n, a]})#L] = new MonadPlus[({type L[+a] = ListT[n, a]})#L] {
        // Functor
        private type f[+a] = ListT[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ListT {
            import _N.`for`
            for { a <- run(m) } yield List.map(f)(a)
        }
        // Monad
        private type m[+a] = ListT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = ListT { _N.`return`(Lazy(List(a))) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ListT {
            import _N.`for`
            for { a <- run(m); b <- _N.mapM(run[n, b]_ compose k)(a) } yield List.concat(b)
        }
        // MonadPlus
        override def mzero: m[Nothing] = ListT { _N.`return`(Nil) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = ListT {
            import _N.`for`
            for { a <- run(m); b <- run(n) } yield a ++: b
        }
    }
}
