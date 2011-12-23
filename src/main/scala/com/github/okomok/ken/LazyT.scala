

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


final case class LazyT[n[+_], +a](override val old: n[Lazy[a]]) extends NewtypeOf[n[Lazy[a]]]


object LazyT extends LazyTOp with LazyTAs {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = LazyT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[Lazy[a]]
    }

    // Overrides
    //
    // MonadTransControl
    protected type t[n[+_], +a] = LazyT[n, a]
    final case class StT[+a](override val old: Lazy[a]) extends NewtypeOf[Lazy[a]]
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = LazyT {
         _N.liftM((a: a) => Lazy.`return`(a)) {
            f {
                new Run {
                    override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                        _U.liftM((x: Lazy[b]) => StT(x))(run(t))
                    }
                }
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = LazyT {
        _N.liftM((St: StT[a]) => St.old)(nSt)
    }
}


private[ken] trait LazyTOp {
    def run[n[+_], a](m: LazyT[n, a]): n[Lazy[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[Lazy[a]] => u[Lazy[b]])(m: LazyT[n, a]): LazyT[u, b] = LazyT { f(run(m)) }
}


private[ken] sealed trait LazyTAs extends MonadTransControl.Deriving0[LazyT, MonadTransControl.AnyMonad] { this: LazyT.type =>
    override protected def deriveMonad[n[+_]](_N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L] = new Monad[({type L[+a] = t[n, a]})#L] {
        // Functor
        private type f[+a] = t[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => LazyT {
            import _N.`for`
            for { a <- run(m) } yield Lazy(f(a.!))
        }
        // Monad
        private type m[+a] = t[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = LazyT { _N.`return`(Lazy(a)) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = LazyT {
            import _N.`for`
            for { a <- run(m) } { run(k(a.!)) }
        }
    }

    override protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override val selfMonad = deriveMonad(_N)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = LazyT {
            _N.callCC { (c: Lazy[a] => n[Lazy[b]]) =>
                run( f( a => LazyT { c(Lazy(a)) } ) )
            }
        }
    }
}
