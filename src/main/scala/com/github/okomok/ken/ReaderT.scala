

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


final case class ReaderT[r, n[+_], +a](override val old: r => n[a]) extends NewtypeOf[r => n[a]] with (r => n[a]) {
    override def apply(x: r): n[a] = old(x)
}


object ReaderT extends ReaderTOp with ReaderTAs with Kind.FunctionLike {
    trait apply[r] extends apply1[r]
    trait apply1[r] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = ReaderT[r, n, a]
    }

    trait apply2[r, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = ReaderT[r, n#apply1, a]
        override type oldtype1[+a] = r => n#apply1[a]
    }
}


private[ken] trait ReaderTOp {
    def run[r, n[+_], a](m: ReaderT[r, n, a]): r => n[a] = m.run

    def map[r, n[+_], u[+_], a, b](f: n[a] => u[b])(m: ReaderT[r, n, a]): ReaderT[r, u, b] = ReaderT { f `.` run(m) }

    def `with`[r, r_, n[+_], a](f: r_ => r)(m: ReaderT[r, n, a]): ReaderT[r_, n, a] = ReaderT { run(m) `.` f }
}


private[ken] sealed trait ReaderTAs extends MonadTransControl.Deriving1[ReaderT, Trivial, Monad.type ^: MonadBaseControl.type ^: MonadCont.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadState.type ^: MonadWriter.type ^: Kind.Nil] { this: ReaderT.type =>
    private type t1[z, n[+_], +a] = ReaderT[z, n, a]
    private type c[z] = Trivial[z]

    override protected def asMonadTransControl[z](_C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] {
        private type t[n[+_], +a] = t1[z, n, a]
        private type r = z
        final case class StT[+a](override val old: a) extends NewtypeOf[a]
        override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = ReaderT { r =>
            f {
                new Run {
                    override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                        _U.liftM((x: b) => StT(x))(run(t)(r): u[b])
                    }
                }
            }
        }
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = ReaderT { _ =>
            _N.liftM((St: StT[a]) => St.old)(nSt)
        }
    }

    override protected def deriveMonad[z, n[+_]](_N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L] = _asMonadReader(_N)

    override protected def deriveMonadCont[z, n[+_]](_N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = new MonadCont[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type r = z
        override val selfMonad: selfMonad = deriveMonad(_N, Trivial.of[r])
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ReaderT { r =>
            _N.callCC { (c: a => n[b]) =>
                run( f( a => ReaderT { s_ => c(a) } ) )(r)
            }
        }
    }

    override protected def deriveMonadWriter[z, n[+_], w](_N: MonadWriter[w, n], _C: c[z]): MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] = new MonadWriter[w, ({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type r = z
        override val selfMonad: selfMonad = deriveMonad(_N, Trivial.of[r])
        override def monoid: monoid = _N.monoid
        override val tell: tell = x => asMonadTransControl(Trivial.of[r]).lift(_N.tell(x))(_N)
        override def listen[a](m: m[a]): m[(a, w)] = ReaderT { w => _N.listen(run(m)(w)) }
        override def pass[a](m: m[(a, w => w)]): m[a] = ReaderT { w => _N.pass(run(m)(w)) }
    }

    implicit def _asMonadReader[r, n[+_]](implicit _N: Monad[n]): MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] {
        // Functor
        private type f[+a] = ReaderT[r, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ReaderT { r => {
            import _N.`for`
            for { a <- run(m)(r) } yield f(a)
        } }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ReaderT { r => _N.`return`(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ReaderT { r => {
            import _N.`for`
            for { a <- run(m)(r) } { run(k(a))(r) }
        } }
        // MonadReader
        override def ask: ask = ReaderT { r => _N.`return`(r) }
        override def local[a](f: r => r)(m: m[a]): m[a] = ReaderT { r => run(m)(f(r)) }
    }
}
