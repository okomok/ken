

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


private[ken] sealed trait ReaderTAs0 extends MonadTrans.Deriving1[ReaderT, Trivial, MonadBase.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadState.type ^: Kind.Nil] { this: ReaderT.type =>
    private type c[x] = Trivial[x]

    override protected def deriveMonadTrans[r](_C: c[r]): MonadTrans[({type L[n[+_], +a] = ReaderT[r, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = ReaderT[r, n, a]})#L] {
        private type t[n[+_], +a] = ReaderT[r, n, a]
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

    override protected def deriveMonad[r, n[+_]](_N: Monad[n], _C: c[r]): Monad[({type L[+a] = ReaderT[r, n, a]})#L] = _asMonadReader(_N)

    implicit def _asMonadCont[r, n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = ReaderT[r, n, a]})#L] = new MonadCont[({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = deriveMonad(i, Trivial.of[r])
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ReaderT { r =>
            i.callCC { (c: a => n[b]) =>
                run( f( a => ReaderT { s_ => c(a) } ) )(r)
            }
        }
    }

    implicit def _asMonadWriter[r, n[+_], w](implicit i: MonadWriter[w, n]): MonadWriter[w, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadWriter[w, ({type L[+a] = ReaderT[r, n, a]})#L] with MonadProxy[({type L[+a] = ReaderT[r, n, a]})#L] {
        private type m[+a] = ReaderT[r, n, a]
        override val selfMonad = deriveMonad(i, Trivial.of[r])
        override def monoid: Monoid[w] = i.monoid
        override val tell: w => m[Unit] = x => deriveMonadTrans(Trivial.of[r]).lift(i.tell(x))
        override def listen[a](m: m[a]): m[(a, w)] = ReaderT { w => i.listen(run(m)(w)) }
        override def pass[a](m: m[(a, w => w)]): m[a] = ReaderT { w => i.pass(run(m)(w)) }
    }
}

private[ken] sealed trait ReaderTAs extends ReaderTAs0 { this: ReaderT.type =>
    implicit def _asMonadReader[r, n[+_]](implicit i: Monad[n]): MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] = new MonadReader[r, ({type L[+a] = ReaderT[r, n, a]})#L] {
        // Functor
        private type f[+a] = ReaderT[r, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ReaderT { r => {
            import i.`for`
            for { a <- run(m)(r) } yield f(a)
        } }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ReaderT { r => i.`return`(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ReaderT { r => {
            import i.`for`
            for { a <- run(m)(r) } { run(k(a))(r) }
        } }
        // MonadReader
        override def ask: m[r] = ReaderT { r => i.`return`(r) }
        override def local[a](f: r => r)(m: m[a]): m[a] = ReaderT { r => run(m)(f(r)) }
    }
}
