

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


final case class ListT[n[+_], +a](override val old: n[List[a]]) extends NewtypeOf[n[List[a]]]


object ListT extends ListTOp with ListTAs with MonadTransControl[ListT] {
    trait apply[n[+_]] extends apply1[n]
    trait apply1[n[+_]] extends Kind.Newtype1 {
        override type apply1[+a] = ListT[n, a]
        override type oldtype1[+a] = n[List[a]]
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], +a] = ListT[n, a]
    override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = ListT {
        import i.`for`
        for { a <- n } yield List(a)
    }
    // MonadTransControl
    override def liftControl[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = ListT {
         i.liftM((x: a) => List.`return`(x)) {
            f {
                new Run {
                    override def apply[n_[+_], o[+_], b](t: t[n_, b], * : Type1[o] = null)(implicit ri: Monad[n_], rj: Monad[o], rk: Monad[({type m[+a] = t[o, a]})#m]): n_[t[o, b]] = {
                        ri.liftM((x: List[b]) => ListT(rj.`return`(x)))(run(t))
                    }
                }
            }
        }
    }
}


private[ken] trait ListTOp {
    def run[n[+_], a](m: ListT[n, a]): n[List[a]] = m.run

    def map[n[+_], u[+_], a, b](f: n[List[a]] => u[List[b]])(m: ListT[n, a]): ListT[u, b] = ListT { f(run(m)) }
}


private[ken] sealed trait ListTAs0 { this: ListT.type =>
    /*
    @Annotation.typeAliasWorkaround
    implicit def _asNewtype1[n[+_]]: Newtype1[apply1[n]#apply1, apply1[n]#oldtype1] = new Newtype1[apply1[n]#apply1, apply1[n]#oldtype1] {
        // private type nt[+a] = ListT[n, a]
        // private type ot[+a] = n[List[a]]
        override def newOf[a](ot: Lazy[n[List[a]]]): ListT[n, a] = ListT(ot)
        override def oldOf[a](nt: Lazy[ListT[n, a]]): n[List[a]] = nt.run
    }
    */
    implicit val _asMonadTrans: MonadTransControl[ListT] = this

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[apply1[n]#apply1] = new MonadCont[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = ListT {
            i.callCC { (c: List[a] => n[List[b]]) =>
                run( f( a => ListT { c(List(a)) } ) )
            }
        }
    }

    implicit def _asMonadError[n[+_], e](implicit i: MonadError[e, n]): MonadError[e, apply1[n]#apply1] = new MonadError[e, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def throwError[a](e: e): m[a] = _asMonadTrans.lift(i.throwError(e))
        override def catchError[a](m: m[a])(h: e => m[a]): m[a] = ListT {
            i.catchError(run(m)) { e => run(h(e)) }
        }
    }

    implicit def _asMonadReader[n[+_], r](implicit i: MonadReader[r, n]): MonadReader[r, apply1[n]#apply1] = new MonadReader[r, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def ask: m[r] = _asMonadTrans.lift(i.ask)
        override def local[a](f: r => r)(m: m[a]): m[a] = ListT { i.local(f)(run(m)) }
    }

    implicit def _asMonadState[n[+_], s](implicit i: MonadState[s, n]): MonadState[s, apply1[n]#apply1] = new MonadState[s, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def get: m[s] = _asMonadTrans.lift(i.get)
        override def put(s: s): m[Unit] = _asMonadTrans.lift(i.put(s))
    }

    implicit def _asMonadIO[n[+_]](implicit i: MonadIO[n]): MonadIO[apply1[n]#apply1] = new MonadIO[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
        override val selfMonad = _asMonadPlus[n]
        override def liftIO[a](io: IO[a]): m[a] = _asMonadTrans.lift(i.liftIO(io))
    }
}

@Annotation.compilerWorkaround("2.9.1") // ambiguous with `_asMonadIO` for some reason.
private[ken] sealed trait ListTAs1 extends ListTAs0 { this: ListT.type =>
    implicit def _asMonadControlIO[n[+_]](implicit i: MonadControlIO[n]): MonadControlIO[apply1[n]#apply1] = new MonadControlIO[apply1[n]#apply1] with MonadIOProxy[apply1[n]#apply1] {
        private type m[+a] = ListT[n, a]
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

private[ken] sealed trait ListTAs extends ListTAs1 { this: ListT.type =>
    implicit def _asMonadPlus[n[+_]](implicit i: Monad[n]): MonadPlus[apply1[n]#apply1] = new MonadPlus[apply1[n]#apply1] {
        // Functor
        private type f[+a] = ListT[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => ListT {
            import i.`for`
            for { a <- run(m) } yield List.map(f)(a)
        }
        // Monad
        private type m[+a] = ListT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = ListT { i.`return`(Lazy(List(a))) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = ListT {
            import i.`for`
            for { a <- run(m); b <- i.mapM(run[n, b]_ compose k)(a) } yield List.concat(b)
        }
        // MonadPlus
        override def mzero: m[Nothing] = ListT { i.`return`(Nil) }
        override def mplus[a](m: m[a])(n: Lazy[m[a]]): m[a] = ListT {
            import i.`for`
            for { a <- run(m); b <- run(n) } yield a ++: b
        }
    }
}
