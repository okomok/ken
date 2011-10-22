

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2007 Magnus Therning
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class IdentityT[n[+_], +a](override val old: n[a]) extends NewtypeOf[n[a]]


object IdentityT extends IdentityTOp with IdentityTAs with MonadTransControl[IdentityT] {
    trait apply[n[+_]] extends apply1[n]
    trait apply1[n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = IdentityT[n, a]
        override type oldtype1[+a] = n[a]
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], +a] = IdentityT[n, a]
    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = IdentityT(n)
    // MonadTransControl
    override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = IdentityT {
         i.liftM((x: a) => x) {
            f {
                new Run {
                    override def apply[n_[+_], o[+_], b](t: t[n_, b], * : TypeC1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                        ri.liftM((x: b) => IdentityT(rj.`return`(x)))(run(t))
                    }
                }
            }
        }
    }
}


private[ken] trait IdentityTOp {
    def run[n[+_], a](m: IdentityT[n, a]): n[a] = m.run

    def map[n[+_], u[+_], a, b](f: n[a] => u[b])(m: IdentityT[n, a]): IdentityT[u, b] = IdentityT { f(run(m)) }

    def lift2[n[+_], u[+_], p[+_], a, b, c](f: n[a] => u[b] => p[c])(a: IdentityT[n, a])(b: IdentityT[u, b]): IdentityT[p, c] = IdentityT { f(run(a))(run(b)) }
}


private[ken] sealed trait IdentityTAs0 { this: IdentityT.type =>
    implicit val _asMonadTrans: MonadTransControl[IdentityT] = this

    implicit def _asMonadPlus[n[+_]](implicit i: MonadPlus[n]): MonadPlus[apply1[n]#apply1] = new MonadPlus[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def mzero: m[Nothing] = IdentityT(i.mzero)
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = IdentityT { i.mplus(run(m))(Lazy(run(n.!))) }
    }

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[apply1[n]#apply1] = new MonadCont[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = IdentityT {
            i.callCC { (c: a => n[b]) =>
                run( f( a => IdentityT { c(a) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, apply1[n]#apply1] = new MonadError[e, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = IdentityT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply1[n]#apply1] = new MonadReader[r, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = IdentityT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, apply1[n]#apply1] = new MonadState[s, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def get: m[s] = _asMonadTrans.lift(i.get)
        override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
    }

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[apply1[n]#apply1] = new MonadIO[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = _asMonad[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait IdentityTAs1 extends IdentityTAs0 { this: IdentityT.type =>
    implicit def _asMonadControlIO[n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[apply1[n]#apply1] = new MonadControlIO[apply1[n]#apply1] with MonadIOProxy[apply1[n]#apply1] {
        private type m[+a] = IdentityT[n, a]
        private val mt = _asMonadTrans
        override val selfMonadIO = _asMonadIO[n]
        override def liftIO[a](io: IO[a]): m[a] = mt.lift(i.liftIO(io))
        override def liftControlIO[a](f: RunInIO => IO[a]): m[a] = {
            mt.liftControl { run1 =>
                i.liftControlIO { runInBase =>
                    f {
                        new RunInIO {
                            override def apply[b](t: m[b]): IO[m[b]] = IO.liftM((x: n[m[b]]) => join(mt.lift(x)))(runInBase(run1(t)))
                        }
                    }
                }
            }
        }
    }
}

private[ken] sealed trait IdentityTAs extends IdentityTAs1 { this: IdentityT.type =>
    implicit def _asMonad[n[+_]](implicit i: Monad[n]): Monad[apply1[n]#apply1] with HighPriority = new Monad[apply1[n]#apply1] with HighPriority {
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
