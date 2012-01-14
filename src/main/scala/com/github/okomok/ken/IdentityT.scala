

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2007 Magnus Therning
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class IdentityT[n[+_], +a](override val old: n[a]) extends NewtypeOf[n[a]]


object IdentityT extends IdentityTOp with IdentityTAs with MonadTransControl[IdentityT] {
    trait apply[n <: Kind.Function1] extends apply1[n]
    trait apply1[n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = IdentityT[n#apply1, a]
        override type oldtype1[+a] = n#apply1[a]
    }

    // Overrides
    //
    // MonadTransControl
    protected type t[n[+_], +a] = IdentityT[n, a]
    override type StT[+a] = a
    override def liftWith[n[+_], a](f: Run => n[a])(implicit _N: Monad[n]): t[n, a] = IdentityT {
        f {
            new Run {
                override def apply[u[+_], b](t: t[u, b])(implicit _U: Monad[u]): u[StT[b]] = run(t)
            }
        }
    }
    override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = IdentityT(nSt)
}


private[ken] trait IdentityTOp {
    def run[n[+_], a](m: IdentityT[n, a]): n[a] = m.run

    def map[n[+_], u[+_], a, b](f: n[a] => u[b])(m: IdentityT[n, a]): IdentityT[u, b] = IdentityT { f(run(m)) }

    def lift2[n[+_], u[+_], p[+_], a, b, c](f: n[a] => u[b] => p[c])(a: IdentityT[n, a])(b: IdentityT[u, b]): IdentityT[p, c] = IdentityT { f(run(a))(run(b)) }
}


private[ken] sealed trait IdentityTAs extends MonadTransControl.Deriving0[IdentityT, MonadWriter.type ^: MonadTransControl.AnyMonad] { this: IdentityT.type =>
    override protected def deriveMonad[n[+_]](_N: Monad[n]): Monad[({type L[+a] = t[n, a]})#L] = new Monad[({type L[+a] = t[n, a]})#L] {
        // Functor
        private type f[+a] = t[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => IdentityT { _N.fmap(f)(run(m)) }
        // Applicative
        override def op_<*>[a, b](f: f[a => b]): f[a] => f[b] = m => IdentityT { _N.op_<*>(run(f))(run(m)) }
        // Monad
        private type m[+a] = IdentityT[n, a]
        override def `return`[a](a: Lazy[a]): m[a] = IdentityT { _N.`return`(a) }
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = IdentityT {
            import _N.>>=
            run(m) >>= (x => run(k(x)))
        }
    }

    override protected def deriveMonadCont[n[+_]](_N: MonadCont[n]): MonadCont[({type L[+a] = t[n, a]})#L] = new MonadCont[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override val selfMonad: selfMonad = deriveMonad(_N)
        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = IdentityT {
            _N.callCC { (c: a => n[b]) =>
                run( f( a => IdentityT { c(a) } ) )
            }
        }
    }

    override protected def deriveMonadWriter[n[+_]](_N: MonadWriter[n]): MonadWriter.Of[_N.WriteType, ({type L[+a] = t[n, a]})#L] = new MonadWriter[({type L[+a] = t[n, a]})#L] with MonadProxy[({type L[+a] = t[n, a]})#L] {
        private type m[+a] = t[n, a]
        override val selfMonad: selfMonad = deriveMonad(_N)
        override type WriteType = _N.WriteType
        override def monoid: monoid = _N.monoid
        override def tell: tell = x => asMonadTrans.lift(_N.tell(x))(_N)
        override def listen[a](x: m[a]): m[(a, WriteType)] = IdentityT { _N.listen(x.old) }
        override def pass[a](x: m[(a, WriteType => WriteType)]): m[a] = IdentityT { _N.pass(x.old) }
    }
}
