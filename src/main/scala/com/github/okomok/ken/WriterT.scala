

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


final case class WriterT[w, n[+_], +a](override val old: n[(a, w)]) extends NewtypeOf[n[(a, w)]]


object WriterT extends WriterTOp with WriterTAs with Kind.FunctionLike {
    trait apply1[w] extends Kind.MonadTrans {
    trait apply[w] extends apply1[w]
        override type monadTrans[n[+_], +a] = WriterT[w, n, a]
    }

    trait apply2[w, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = WriterT[w, n#apply1, a]
        override type oldtype1[+a] = n#apply1[(a, w)]
    }
}


private[ken] trait WriterTOp {
    def run[w, n[+_], a](n: WriterT[w, n, a]): n[(a, w)] = n.run

    def exec[w, n[+_], a](n: WriterT[w, n, a])(implicit i: Monad[n]): n[w] = {
        import i.`for`
        for { (_, w) <- run(n) } yield w
    }

    def map[w, w_, n[+_], u[+_], a, b](f: n[(a, w)] => u[(b, w_)])(m: WriterT[w, n, a]): WriterT[w_, u, b] = WriterT { f(run(m)) }
}


private[ken] sealed trait WriterTAs extends MonadTransControl.Deriving1[WriterT, Monoid, Monad.type ^: MonadBaseControl.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil] { this: WriterT.type =>
    private type t1[z, n[+_], +a] = WriterT[z, n, a]
    private type c[z] = Monoid[z]

    override protected def asMonadTransControl[z](_C: c[z]): MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] = new MonadTransControl[({type L[n[+_], +a] = t1[z, n, a]})#L] {
        private type t[n[+_], +a] = t1[z, n, a]
        private type w = z
        override type StT[+a] = (a, w)
        override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = WriterT {
            _N.liftM((x: a) => (x, _C.mempty)) {
                f {
                    new Run {
                        override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = run(t)
                    }
                }
            }
        }
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = WriterT(nSt)
    }

    override protected def deriveMonad[z, n[+_]](_N: Monad[n], _C: c[z]): Monad[({type L[+a] = t1[z, n, a]})#L] = _asMonadWriter(_N, _C)

    override protected def deriveMonadCont[z, n[+_]](_N: MonadCont[n], _C: c[z]): MonadCont[({type L[+a] = t1[z, n, a]})#L] = new MonadCont[({type L[+a] = t1[z, n, a]})#L] with MonadProxy[({type L[+a] = t1[z, n, a]})#L] {
        private type m[+a] = t1[z, n, a]
        private type w = z
        override val selfMonad: selfMonad = deriveMonad(_N, _C)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = WriterT {
            _N.callCC { (c: ((a, w)) => n[(b, w)]) =>
                run( f(a => WriterT { c((a, _C.mempty)) }) )
            }
        }
    }

    implicit def _asMonadWriter[w, n[+_]](implicit _N: Monad[n], _C: Monoid[w]): MonadWriter[w, ({type L[+a] = WriterT[w, n, a]})#L] = new MonadWriter[w, ({type L[+a] = WriterT[w, n, a]})#L] {
        // Functor
        private type f[+a] = WriterT[w, n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => WriterT {
            import _N.`for`
            for { (a, w) <- run(m) } yield (f(a), w)
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](a: Lazy[a]): m[a] = WriterT { _N.`return`(a.!, _C.mempty) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = WriterT {
            import _N.`for`
            for { (a, w) <- run(m); (b, w_) <- run(k(a)) } yield (b, _C.mappend(w)(w_))
        }
        // MonadWriter
        override def monoid: Monoid[w] = _C
        override val tell: w => m[Unit] = w => WriterT { _N.`return`((), w) }
        override def listen[a](m: m[a]): m[(a, w)] = WriterT {
            import _N.`for`
            for { (a, w) <- run(m) } yield ((a, w), w)
        }
        override def pass[a](m: m[(a, w => w)]): m[a] = WriterT {
            import _N.`for`
            for { ((a, f), w) <- run(m) } yield (a, f(w))
        }
    }
}
