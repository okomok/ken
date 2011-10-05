

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


final case class MaybeT[n[+_], +a](override val get: n[Maybe[a]]) extends NewtypeOf[n[Maybe[a]]]


object MaybeT extends MaybeTOp with MaybeTAs with MonadTransX[MaybeT] {
    sealed trait apply1[n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = MaybeT[n, a]
        override type oldtype1[+a] = n[Maybe[a]]
    }
    type apply[n[+_]] = apply1[n]

    // Overrides
    //
    // MonadTransX
    private type t[n[+_], +a] = MaybeT[n, a]
    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = MaybeT {
        import i.`for`
        for { a <- n } yield Just(a)
    }
}


private[ken] trait MaybeTOp {
    def run[n[+_], a](m: MaybeT[n, a]): n[Maybe[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[Maybe[a]] => u[Maybe[b]])(m: MaybeT[n, a]): MaybeT[u, b] = MaybeT { f(run(m)) }
}


private[ken] sealed trait MaybeTAs0 { this: MaybeT.type =>
    /*
    @Annotation.typeAliasWorkaround
    implicit def _asNewtype1[n[+_]]: Newtype1[apply1[n]#apply1, apply1[n]#oldtype1] = new Newtype1[apply1[n]#apply1, apply1[n]#oldtype1] {
        // private type nt[+a] = MaybeT[n, a]
        // private type ot[+a] = n[Maybe[a]]
        override def newOf[a](ot: Lazy[n[Maybe[a]]]): MaybeT[n, a] = MaybeT(ot)
        override def oldOf[a](nt: Lazy[MaybeT[n, a]]): n[Maybe[a]] = nt.run
    }
    */

    implicit val _asMonadTrans: MonadTransX[MaybeT] = this

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[apply1[n]#apply1] = new MonadIO[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[apply1[n]#apply1] = new MonadCont[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = MaybeT {
            i.callCC { (c: Maybe[a] => n[Maybe[b]]) =>
                run( f( a => MaybeT { c(Just(a)) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, apply1[n]#apply1] = new MonadError[e, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = MaybeT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply1[n]#apply1] = new MonadReader[r, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = MaybeT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, apply1[n]#apply1] = new MonadState[s, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = MaybeT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def get: m[s] = _asMonadTrans.lift(i.get)
        override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
    }
}

private[ken] trait MaybeTAs extends MaybeTAs0 { this: MaybeT.type =>
    implicit def _asMonadPlus[n[+_]](implicit i: Monad[n]): MonadPlus[apply1[n]#apply1] = new MonadPlus[apply1[n]#apply1] {
        // Monad
        private type m[+a] = MaybeT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = MaybeT { i.`return`(Just(a.!).up) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = MaybeT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => i.`return`(Nothing.of[b])
                    case Just(r) => run(k(r))
                }
            } yield *
        }
        // MonadPlus
        override def mzero: m[Nothing] = MaybeT { i.`return`(Nothing) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = MaybeT {
            import i.`for`
            for {
                a <- run(m)
                * <- a match {
                    case Nothing => run(n)
                    case Just(r) => i.`return`(Just(r))
                }
            } yield *
        }
    }
}
