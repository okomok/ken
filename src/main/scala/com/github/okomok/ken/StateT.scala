

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


final case class StateT[s, n[+_], +a](override val old: s => n[(a, s)]) extends NewtypeOf[s => n[(a, s)]] with (s => n[(a, s)]) {
    override def apply(x: s): n[(a, s)] = old(x)
}


object StateT extends StateTOp with StateTAs with Kind.FunctionLike {
    trait apply[s] extends apply1[s]
    trait apply1[s] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = StateT[s, n, a]
    }

    trait apply2[s, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = StateT[s, n#apply1, a]
        override type oldtype1[+a] = s => n#apply1[(a, s)]
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


private[ken] sealed trait StateTAs extends MonadTransControl.Deriving1[StateT, Trivial, Monad.type ^: MonadBaseControl.type ^: MonadCont.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadWriter.type ^: Kind.Nil] { this: StateT.type =>
    private type t1[z, n[+_], +a] = StateT[z, n, a]
    private type c[z] = Trivial[z]

    override protected def asMonadTransControl[z](_C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] {
        private type t[n[+_], +a] = t1[z, n, a]
        private type s = z
        final case class StT[+a](override val old: (a, s)) extends NewtypeOf[(a, s)]
        override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = StateT { s =>
            _N.liftM((x: a) => (x, s)) {
                f {
                    new Run {
                        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                            _U.liftM((x: ((b, s))) => StT(x))(run(t)(s))
                        }
                    }
                }
            }
        }
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = StateT { _ =>
            _N.liftM((St: StT[a]) => St.old)(nSt)
        }
    }

    override protected def deriveMonad[z, n[+_]](_N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L] = _asMonadState(_N)

    override protected def deriveMonadCont[z, n[+_]](_N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = new MonadCont[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type s = z
        override val selfMonad: selfMonad = deriveMonad(_N, Trivial.of[s])
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
            _N.callCC { (c: ((a, s)) => n[(b, s)]) =>
                run( f( a => StateT { s_ => c((a, s_)) } ) )(s)
            }
        }
    }

    override protected def deriveMonadWriter[z, n[+_], w](_N: MonadWriter[w, n], _C: c[z]): MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] = new MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type s = z
        override val selfMonad: selfMonad = deriveMonad(_N, Trivial.of[s])
        override def monoid: monoid = _N.monoid
        override val tell: tell = x => asMonadTransControl(Trivial.of[s]).lift(_N.tell(x))(_N)
        override def listen[a](m: m[a]): m[(a, w)] = StateT { s => {
            import _N.`for`
            for { ((a, s_), w) <- _N.listen(run(m)(s)) } yield ((a, w), s_)
        } }
        override def pass[a](m: m[(a, w => w)]): m[a] = StateT { s =>
            import _N.`for`
            _N.pass {
                for { ((a, f), s_) <- run(m)(s) } yield ((a, s_), f)
            }
        }
    }

    implicit def _asMonadState[s, n[+_]](implicit _N: Monad[n]): MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] = new MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] {
        // Functor
        private type f[+a] = StateT[s, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => StateT { s =>
            import _N.`for`
            for { (x, s_) <- run(m)(s) } yield (f(x), s_)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = StateT { s => _N.`return`(a.!, s) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
            import _N.`for`
            for { (a, s_) <- run(m)(s) } { run(k(a))(s_) }
        }
        // MonadState
        override val get: m[s] = StateT { s => _N.`return`(s, s) }
        override val put: s => m[Unit] = s => StateT { _ => _N.`return`((), s) }
    }
}
