

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


final case class ContT[r, n[+_], +a](override val old: (a => n[r]) => n[r]) extends NewtypeOf[(a => n[r]) => n[r]] with ((a => n[r]) => n[r]) {
    override def apply(x: a => n[r]): n[r] = old(x)
}


object ContT extends ContTOp with ContTAs with Kind.FunctionLike {
    trait apply[r] extends apply1[r]
    trait apply1[r] extends Kind.MonadTrans {
        override type monadTrans[n[+_], +a] = ContT[r, n, a]
    }

    trait apply2[r, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = ContT[r, n#apply1, a]
        override type oldtype1[+a] = r => n#apply1[a]
    }
}


private[ken] trait ContTOp {
    def run[r, n[+_], a](m: ContT[r, n, a]): (a => n[r]) => n[r] = m.run

    def map[r, n[+_], a](f: n[r] => n[r])(m: ContT[r, n, a]): ContT[r, n, a] = ContT { f `.` run(m) }

    def `with`[r, n[+_], a, b](f: (b => n[r]) => (a => n[r]))(m: ContT[r, n, a]): ContT[r, n, b] = ContT { run(m) `.` f }
}


private[ken] sealed trait ContTAs0 { this: ContT.type =>
    implicit def _asMonadTrans[r]: MonadTrans[apply1[r]#monadTrans] = new MonadTrans[apply1[r]#monadTrans] {
        // MonadTrans
        private type t[n[+_], +a] = ContT[r, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = ContT { c => i.op_>>=(n)(c) }
    }

    implicit def _asMonadReader[r, n[+_], r_](implicit i: MonadReader[r_, n]): MonadReader[r_, ({type L[+a] = ContT[r, n, a]})#L] = new MonadReader[r_, ({type L[+a] = ContT[r, n, a]})#L] with MonadProxy[({type L[+a] = ContT[r, n, a]})#L] {
        private type m[+a] = ContT[r, n, a]
        override val selfMonad = _asMonadCont[r, n]
        override def ask: m[r_] = _asMonadTrans[r].lift(i.ask)
        override def local[a](f: r_ => r_)(m: m[a]): m[a] = ContT { c =>
            import i.`for`
            for {
                r <- i.ask
            } {
                i.local(f)(run(m)(i.local[r](const(r))_ `.` c))
            }
        }
    }

    implicit def _asMonadState[r, n[+_], s](implicit i: MonadState[s, n]): MonadState[s, ({type L[+a] = ContT[r, n, a]})#L] = new MonadState[s, ({type L[+a] = ContT[r, n, a]})#L] with MonadProxy[({type L[+a] = ContT[r, n, a]})#L] {
        private type m[+a] = ContT[r, n, a]
        private val mt = _asMonadTrans[r]
        override val selfMonad = _asMonadCont[r, n]
        override def get: m[s] = mt.lift(i.get)
        override def put(s: s): m[Unit] = mt.lift(i.put(s))
    }

    implicit def _asMonadIO[r, n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = ContT[r, n, a]})#L] = new MonadIO[({type L[+a] = ContT[r, n, a]})#L] with MonadProxy[({type L[+a] = ContT[r, n, a]})#L] {
        private type m[+a] = ContT[r, n, a]
        private val mt = _asMonadTrans[r]
        override val selfMonad = _asMonadCont[r, n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
    }
}


private[ken] sealed trait ContTAs extends ContTAs0 { this: ContT.type =>
    implicit def _asMonadCont[r, n[+_]](implicit i: Monad[n]): MonadCont[({type L[+a] = ContT[r, n, a]})#L] = new MonadCont[({type L[+a] = ContT[r, n, a]})#L] {
        // Functor
        private type f[+a] = ContT[r, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ContT { c => run(m)(c `.` f) }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = ContT { c => c(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ContT { c => run(m)(a => run(k(a))(c)) }
        // MonadCont
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ContT { c => run( f( a => ContT { _ => c(a) } ) )(c) }
    }
}
