

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


final case class StateT[s, n[+_], +a](override val get: s => n[(a, s)]) extends NewtypeOf[s => n[(a, s)]]


object StateT extends StateTOp with StateTAs with Kind.FunctionLike {
    sealed trait apply1[s] extends Kind.MonadTransX {
        override type monadTrans[n[+_], +a] = StateT[s, n, a]
    }
    type apply[s] = apply1[s]

    sealed trait apply2[s, n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = StateT[s, n, a]
        override type oldtype1[+a] = s => n[(a, s)]
    }
}


private[ken] trait StateTOp {
    def run[s, n[+_], a](m: StateT[s, n, a]): s => n[(a, s)] = m.run

    def eval[s, n[+_], a](m: StateT[s, n, a])(s: s)(implicit i: Monad[n]): n[a] = {
        import i.`for`
        for { (a, _) <- run(m)(s) } yield a
    }

    def exec[s, n[+_], a](m: StateT[s, n, a])(s: s)(implicit i: Monad[n]): n[s] = {
        import i.`for`
        for { (_, s) <- run(m)(s) } yield s
    }

    def map[s, n[+_], u[+_], a, b](f: n[(a, s)] => u[(b, s)])(m: StateT[s, n, a]): StateT[s, u, b] = StateT { f `.` run(m) }

    def `with`[s, n[+_], a](f: s => s)(m: StateT[s, n, a]): StateT[s, n, a] = StateT { run(m) `.` f }
}


private[ken] sealed trait StateTAs0 { this: StateT.type =>
    implicit def _asNewtype1[s, n[+_]]: Newtype1[apply2[s, n]#apply1, apply2[s, n]#oldtype1] = new Newtype1[apply2[s, n]#apply1, apply2[s, n]#oldtype1] {
        private type nt[+a] = StateT[s, n, a]
        private type ot[+a] = s => n[(a, s)]
        override def newOf[a](ot: Lazy[ot[a]]): nt[a] = StateT(ot)
        override def oldOf[a](nt: Lazy[nt[a]]): ot[a] = nt.run
    }

    implicit def _asMonadTrans[s]: MonadTransX[apply1[s]#monadTrans] = new MonadTransX[apply1[s]#monadTrans] {
        private type t[n[+_], +a] = StateT[s, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = StateT { s => {
            import i.`for`
            for { a <- n } yield (a, s)
        } }
    }

    implicit def _asMonadPlus[s, n[+_]](implicit i: MonadPlus[n]): MonadPlus[apply2[s, n]#apply1] = new MonadPlus[apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def mzero: m[Nothing] = StateT { _ => i.mzero }
        override def mplus[a](m1: m[a])(m2: Lazy[m[a]]): m[a] = StateT { s =>
            i.mplus(run(m1)(s))(run(m2)(s))
        }
    }

    implicit def _asMonadFix[s, n[+_]](implicit i: MonadFix[n]): MonadFix[apply2[s, n]#apply1] = new MonadFix[apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def mfix[a](f: Lazy[a] => m[a]): m[a] = StateT { s =>
            i.mfix { (aI_ : Lazy[(a, s)]) => run(f(aI_._1))(s) }
        }
    }

    implicit def _asMonadIO[s, n[+_]](implicit i: MonadIO[n]): MonadIO[apply2[s, n]#apply1] = new MonadIO[apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }

    implicit def _asMonadCont[s, n[+_]](implicit i: MonadCont[n]): MonadCont[apply2[s, n]#apply1] = new MonadCont[apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
            i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                run( f( a => StateT { s_ => c((a, s_)) } ) )(s)
            }
        }
    }

    implicit def _asMonadError[s, n[+_], e](implicit i: MonadError[e, n]): MonadError[e, apply2[s, n]#apply1] = new MonadError[e, apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = StateT { s =>
            i.catchError(run(m)(s)) { e => run(h(e))(s) }
        }
    }

    implicit def _asMonadReader[s, n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply2[s, n]#apply1] = new MonadReader[r, apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = StateT { s => i.local(f)(run(m)(s)) }
    }

    implicit def _asMonadWriter[s, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, apply2[s, n]#apply1] = new MonadWriter[w, apply2[s, n]#apply1] with MonadProxy[apply2[s, n]#apply1] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = _asMonadState[s, n]
        override def monoid: Monoid[w] = i.monoid
        override def tell(x: w): m[Unit] = _asMonadTrans.lift(i.tell(x))
        override def listen[a](m: m[a]): m[(a, w)] = StateT { s => {
            import i.`for`
            for { ((a, s_), w) <- i.listen(run(m)(s)) } yield ((a, w), s_)
        } }
        override def pass[a](m: m[(a, w => w)]): m[a] = StateT { s =>
            import i.`for`
            i.pass {
                for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
            }
        }
    }
}

private[ken] sealed trait StateTAs extends StateTAs0 { this: StateT.type =>
    implicit def _asMonadState[s, n[+_]](implicit i: Monad[n]): MonadState[s, apply2[s, n]#apply1] = new MonadState[s, apply2[s, n]#apply1] {
        // Functor
        private type f[+a] = StateT[s, n, a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = StateT { s =>
            import i.`for`
            for { (x, s_) <- run(m)(s) } yield (f(x), s_)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = StateT { s => i.`return`(a.!, s) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
            import i.`for`
            for { (a, s_) <- run(m)(s); * <- run(k(a))(s_) } yield *
        }
        // MonadState
        override def get: m[s] = StateT { s => i.`return`(s, s) }
        override def put(s: s): m[Unit] = StateT { _ => i.`return`((), s) }
    }
}
