

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
    private type t[n[+_], +a] = MaybeT[n, a]
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


private[ken] sealed trait MaybeTAs0 { this: MaybeT.type =>
    /*
    @Annotation.typeAliasWorkaround
    implicit def _asNewtype1[n[+_]]: Newtype1[({type L[+a] = MaybeT[n, a]})#L, apply1[n]#oldtype1] = new Newtype1[({type L[+a] = MaybeT[n, a]})#L, apply1[n]#oldtype1] {
        // private type nt[+a] = MaybeT[n, a]
        // private type ot[+a] = n[Maybe[a]]
        override def newOf[a](ot: Lazy[n[Maybe[a]]]): MaybeT[n, a] = MaybeT(ot)
        override def oldOf[a](nt: Lazy[MaybeT[n, a]]): n[Maybe[a]] = nt.run
    }
    */

    implicit val _asMonadTrans: MonadTrans[MaybeT] = this

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = MaybeT[n, a]})#L] = new MonadCont[({type L[+a] = MaybeT[n, a]})#L] with MonadProxy[({type L[+a] = MaybeT[n, a]})#L] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = MaybeT {
            i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                run( f( a => MaybeT { c(Just(a)) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, ({type L[+a] = MaybeT[n, a]})#L] = new MonadError[e, ({type L[+a] = MaybeT[n, a]})#L] with MonadProxy[({type L[+a] = MaybeT[n, a]})#L] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = MaybeT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, ({type L[+a] = MaybeT[n, a]})#L] = new MonadReader[r, ({type L[+a] = MaybeT[n, a]})#L] with MonadProxy[({type L[+a] = MaybeT[n, a]})#L] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override val ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = MaybeT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, ({type L[+a] = MaybeT[n, a]})#L] = new MonadState[s, ({type L[+a] = MaybeT[n, a]})#L] with MonadProxy[({type L[+a] = MaybeT[n, a]})#L] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override val get: m[s] = _asMonadTrans.lift(i.get)
        override val put: s => m[Unit] = s => _asMonadTrans.lift(i.put(s))
    }

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = MaybeT[n, a]})#L] = new MonadIO[({type L[+a] = MaybeT[n, a]})#L] with MonadProxy[({type L[+a] = MaybeT[n, a]})#L] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }

    implicit def _asMonadBase[n[+_], b[+_]](implicit _N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = MaybeT[n, a]})#L] = new MonadBaseProxy[b, ({type L[+a] = MaybeT[n, a]})#L] {
        type t[n[+_], +a] = MaybeT[n, a]
        override val selfMonadBase = new MonadBase.TransDefault[t, n, b](_asMonadTrans, _N, _asMonadPlus(_N))
    }
}

private[ken] sealed trait MaybeTAs extends MaybeTAs0 { this: MaybeT.type =>
    implicit def _asMonadPlus[n[+_]](implicit i: Monad[n]): MonadPlus[({type L[+a] = MaybeT[n, a]})#L] = new MonadPlus[({type L[+a] = MaybeT[n, a]})#L] {
        // Monad
        private type m[+a] = MaybeT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = MaybeT { i.`return`(Just(a.!).up) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = MaybeT {
            import i.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Nothing => i.`return`(Nothing.of[b])
                    case Just(r) => run(k(r))
                }
            }
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { i.`return`(Nothing) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = MaybeT {
            import i.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Nothing => run(n)
                    case Just(r) => i.`return`(Just(r))
                }
            }
        }
    }
}
