

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


private[ken] sealed trait IdentityTAs0 extends MonadTrans.Deriving0[IdentityT, MonadBase.type ^: MonadError.type ^: MonadFix.type ^: MonadIO.type ^: MonadPlus.type ^: MonadReader.type ^: MonadState.type ^: Kind.Nil] { this: IdentityT.type =>
    override protected def deriveMonad[n[+_]](_N: Monad[n]) = _asMonad(_N)

    implicit def _asMonadCont[n[+_]](implicit i: MonadCont[n]): MonadCont[({type L[+a] = IdentityT[n, a]})#L] = new MonadCont[({type L[+a] = IdentityT[n, a]})#L] with MonadProxy[({type L[+a] = IdentityT[n, a]})#L] {
        private type m[+a] = IdentityT[n, a]
        override val selfMonad = deriveMonad(i)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = IdentityT {
            i.callCC { (c: a => n[b]) =>
                run( f( a => IdentityT { c(a) } ) )
            }
        }
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
