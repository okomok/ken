

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


private[ken] sealed trait StateTAs0 extends MonadTrans.Deriving1[StateT, Trivial, MonadBase.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: Kind.Nil] { this: StateT.type =>
    private type c[x] = Trivial[x]

    override protected def deriveMonadTrans[s](_C: c[s]): MonadTrans[({type L[n[+_], +a] = StateT[s, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = StateT[s, n, a]})#L] {
        private type t[n[+_], +a] = StateT[s, n, a]
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

    override protected def deriveMonad[s, n[+_]](_N: Monad[n], _C: c[s]): Monad[({type L[+a] = StateT[s, n, a]})#L] = _asMonadState(_N)

    implicit def _asMonadCont[s, n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = StateT[s, n, a]})#L] = new MonadCont[({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = deriveMonad(i, Trivial.of[s])
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = StateT { s =>
            i.callCC { (c: ((a, s)) => n[(b, s)]) =>
                run( f( a => StateT { s_ => c((a, s_)) } ) )(s)
            }
        }
    }

    implicit def _asMonadWriter[s, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = StateT[s, n, a]})#L] = new MonadWriter[w, ({type L[+a] = StateT[s, n, a]})#L] with MonadProxy[({type L[+a] = StateT[s, n, a]})#L] {
        private type m[+a] = StateT[s, n, a]
        override val selfMonad = deriveMonad(i, Trivial.of[s])
        override def monoid: Monoid[w] = i.monoid
        override val tell: w => m[Unit] = x => deriveMonadTrans(Trivial.of[s]).lift(i.tell(x))
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
    implicit def _asMonadState[s, n[+_]](implicit i: Monad[n]): MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] = new MonadState[s, ({type L[+a] = StateT[s, n, a]})#L] {
        // Functor
        private type f[+a] = StateT[s, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => StateT { s =>
            import i.`for`
            for { (x, s_) <- run(m)(s) } yield (f(x), s_)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = StateT { s => i.`return`(a.!, s) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = StateT { s =>
            import i.`for`
            for { (a, s_) <- run(m)(s) } { run(k(a))(s_) }
        }
        // MonadState
        override val get: m[s] = StateT { s => i.`return`(s, s) }
        override val put: s => m[Unit] = s => StateT { _ => i.`return`((), s) }
    }
}
