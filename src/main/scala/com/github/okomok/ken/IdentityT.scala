

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2007 Magnus Therning
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class IdentityT[n[+_], +a](override val old: n[a]) extends NewtypeOf[n[a]]


object IdentityT extends IdentityTOp with IdentityTAs with MonadTrans[IdentityT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = IdentityT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[a]
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], +a] = IdentityT[n, a]
    final case class StT[+a](override val old: a) extends NewtypeOf[a]
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = IdentityT {
        f {
            new Run {
                override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = {
                    _U.liftM((x: b) => StT(x))(run(t))
                }
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = IdentityT {
        _N.liftM((St: StT[a]) => St.old)(nSt)
    }
}


private[ken] trait IdentityTOp {
    def run[n[+_], a](m: IdentityT[n, a]): n[a] = m.run

    def map[n[+_], u[+_], a, b](f: n[a] => u[b])(m: IdentityT[n, a]): IdentityT[u, b] = IdentityT { f(run(m)) }

    def lift2[n[+_], u[+_], p[+_], a, b, c](f: n[a] => u[b] => p[c])(a: IdentityT[n, a])(b: IdentityT[u, b]): IdentityT[p, c] = IdentityT { f(run(a))(run(b)) }
}


private[ken] sealed trait IdentityTAs0 { this: IdentityT.type =>
    implicit val _asMonadTrans: MonadTrans[IdentityT] = this

    implicit def _asMonadPlus[n[+_]](implicit i: MonadPlus[n]): MonadPlus[({type L[+a] = IdentityT[n, a]})#L] = new MonadPlus[({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override val mzero: m[Nothing] = IdentityT(i.mzero)
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = IdentityT { i.mplus(run(m))(Lazy(run(n.!))) }
    }

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = IdentityT[n, a]})#L] = new MonadCont[({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = IdentityT {
            i.callCC { (c: a => n[b]) =>
                run( f( a => IdentityT { c(a) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, ({type L[+a] = IdentityT[n, a]})#L] = new MonadError[e, ({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = IdentityT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, ({type L[+a] = IdentityT[n, a]})#L] = new MonadReader[r, ({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override val ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = IdentityT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, ({type L[+a] = IdentityT[n, a]})#L] = new MonadState[s, ({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override val get: m[s] = _asMonadTrans.lift(i.get)
        override val put: s => m[Unit] = s => _asMonadTrans.lift(i.put(s))
    }

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[({type L[+a] = IdentityT[n, a]})#L] = new MonadIO[({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }

    implicit def _asMonadBase[n[+_], b[+_]](implicit _N: MonadBase[b, n]): MonadBase[b, ({type L[+a] = IdentityT[n, a]})#L] = new MonadBaseProxy[b, ({type L[+a] = IdentityT[n, a]})#L] {
        type t[n[+_], +a] = IdentityT[n, a]
        override val selfMonadBase = new MonadBase.TransDefault[t, n, b](_asMonadTrans, _N, _asMonad(_N))
/*
        private type m[+a] = IdentityT[n, a]
        val _T = _asMonadTrans
        override val selfMonad = _asMonad(_N)
        override val baseMonad = _N.baseMonad
        final case class StM[+a](override val old: _N.StM[_T.StT[a]]) extends NewtypeOf[_N.StM[_T.StT[a]]]
        override def liftBaseWith[a](f: RunInBase => b[a]): m[a] = {
            _T.liftWith { run1 =>
                _N.liftBaseWith { runInBase =>
                    f {
                        new RunInBase {
                            override def apply[c](m: m[c]): b[StM[c]] = baseMonad.liftM((x: _N.StM[_T.StT[c]]) => StM(x))(runInBase(run1(m)))
                        }
                    }
                }
            }
        }
        override def restoreM[a](St: StM[a]): m[a] = _T.restoreT(_N.restoreM(St.old))
*/
    }
}

private[ken] sealed trait IdentityTAs extends IdentityTAs0 { this: IdentityT.type =>
    implicit def _asMonad[n[+_]](implicit i: Monad[n]): Monad[({type L[+a] = IdentityT[n, a]})#L] with HighPriority = new Monad[({type L[+a] = IdentityT[n, a]})#L] with HighPriority {
        // Functor
        private type f[+a] = IdentityT[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => IdentityT { i.fmap(f)(run(m)) }
        // Applicative
        override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = m => IdentityT { i.op_<*>(run(f))(run(m)) }
        // Monad
        private type m[+a] = IdentityT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = IdentityT { i.`return`(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = IdentityT {
            import i.>>=
            run(m) >>= (x => run(k(x)))
        }
    }
}
