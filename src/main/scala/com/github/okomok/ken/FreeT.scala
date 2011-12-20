

// Copyright Shunsuke Sogame 2011.
//
// https://github.com/pepeiborra/control-monad-free
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


final case class FreeT[f[+_], n[+_], +a](override val old: n[Either[a, f[FreeT[f, n, a]]]]) extends NewtypeOf[n[Either[a, f[FreeT[f, n, a]]]]]


object FreeT extends FreeTOp with FreeTAs with Kind.FunctionLike {
    trait apply[f <: Kind.Function1] extends apply1[f]
    trait apply1[f <: Kind.Function1] extends Kind.MonadTrans with Kind.MonadFree {
        override type monadTrans[n[+_], +a] = FreeT[f#apply1, n, a]
        override type functor[+a] = f#apply1[a]
    }

    trait apply2[f <: Kind.Function1, n <: Kind.Function1] extends Kind.Newtype1 {
        override type apply1[+a] = FreeT[f#apply1, n#apply1, a]
        override type oldtype1[+a] = n#apply1[Either[a, f#apply1[FreeT[f#apply1, n#apply1, a]]]]
    }
}


private[ken] trait FreeTOp {
    def run[f[+_], n[+_], a](m: FreeT[f, n, a]): n[Either[a, f[FreeT[f, n, a]]]] = m.run

    def fold[f[+_], n[+_], a, b](p: a => n[b])(i: f[b] => n[b])(m: FreeT[f, n, a])(implicit fT: Traversable[f], nM: Monad[n]): n[b] = {
        import nM.{`for`, >>=}
        for {
            r <- run(m)
        } {
            r match {
                case Left(x) => p(x)
                case Right(fx) => fT.mapM(fold(p)(i))(fx) >>= i
            }
        }
    }

    def `fold'`[f[+_], n[+_], a, b](p: a => b)(i: f[b] => b)(m: FreeT[f, n, a])(implicit fT: Traversable[f], nM: Monad[n]): n[b] = {
        val f: Either[a, f[FreeT[f, n, a]]] => n[b] = {
            case Left(x) => nM.`return`(p(x))
            case Right(fx) => nM.liftM(i)(fT.mapM((n_ : FreeT[f, n, a]) => `fold'`(p)(i)(n_))(fx))
        }
        import nM.>>=
        run(m) >>= f
    }

    trait Map[n[+_], n_[+_]] {
        def apply[a](n: n[a]): n_[a]
    }

    def map[f[+_], n[+_], a, n_[+_]](f: Map[n, n_])(m: FreeT[f, n, a])(implicit fT: Traversable[f], nM: Monad[n]): FreeT[f, n_, a] = FreeT {
        val mapf: FreeT[f, n, a] => FreeT[f, n_, a] = map(f)
        f( nM.fmap(Functor[Either.apply1[a]].fmap(fT.fmap(mapf)))(run(m)) )
    }

    private[ken] def editEither[a, b, c, d](f: a => c)(g: b => d): Either[a, b] => Either[c, d] = Either.either((a: a) => Left(f(a)).of[c, d])((b: b) => Right(g(b)).of[c, d])

    private[ken] def conj[f[+_], n[+_], a, f_[+_], n_[+_], a_](f: n[Either[a, f[FreeT[f, n, a]]]] => n_[Either[a_, f_[FreeT[f_, n_, a_]]]]): FreeT[f, n, a] => FreeT[f_, n_, a_] = x => FreeT(f(run(x)))
}


private[ken] sealed trait FreeTAs0 { this: FreeT.type =>
    implicit def _asFunctor[f[+_], n[+_]](implicit fF: Functor[f], nM: Monad[n]): Functor[({type L[+a] = FreeT[f, n, a]})#L] = new Functor[({type L[+a] = FreeT[f, n, a]})#L] {
        private type g[+a] = FreeT[f, n, a]
        override def fmap[a, b](g: a => b): g[a] => g[b] = conj { nM.fmap(editEither(g)(fF.fmap(p => fmap(g)(p)))) }
    }

    implicit def _asMonadTrans[f[+_]]: MonadTrans[({type L[n[+_], +a] = FreeT[f, n, a]})#L] = new MonadTrans[({type L[n[+_], +a] = FreeT[f, n, a]})#L] {
        private type t[n[+_], +a] = FreeT[f, n, a]
        override def lift[n[+_], a](n: n[a])(implicit i: Monad[n]): t[n, a] = FreeT {
            import i.`for`
            for { a <- n } yield Left(a)
        }
        override def liftWith[n[+_], a](f: Run => n[a])(implicit i: Monad[n]): t[n, a] = error("todo")
        override def restoreT[n[+_], a](nSt: n[StT[a]])(implicit _N: Monad[n]): t[n, a] = error("todo")
    }
}

private[ken] sealed trait FreeTAs extends FreeTAs0 { this: FreeT.type =>
    implicit def _asTraversable[g[+_], n[+_]](implicit nT: Traversable[n], gT: Traversable[g]): Traversable[({type L[+a] = FreeT[g, n, a]})#L] = new Traversable[({type L[+a] = FreeT[g, n, a]})#L] {
        private type t[+a] = FreeT[g, n, a]
        override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit fA: Applicative[f]): f[t[b]] = {
            import fA.`for`
            val f_ : Either[a, g[FreeT[g, n, a]]] => f[Either[b, g[FreeT[g, n, b]]]] = {
                case Left(x) => for { p <- f(x) } yield Left(p)
                case Right(x) => for { p <- gT.traverse(traverse(f))(x) } yield Right(p)
            }
            for { p <- nT.traverse(f_)(run(t)) } yield FreeT(p)
        }
    }

    implicit def _asMonadFree[f[+_], n[+_]](implicit fF: Functor[f], nM: Monad[n]): MonadFree[f, ({type L[+a] = FreeT[f, n, a]})#L] = new MonadFree[f, ({type L[+a] = FreeT[f, n, a]})#L] with FunctorProxy[({type L[+a] = FreeT[f, n, a]})#L] {
        override val selfFunctor = _asFunctor(fF, nM)
        // Monad
        private type m[+a] = FreeT[f, n, a]
        override def `return`[a](a: Lazy[a]): m[a] = FreeT(nM.`return`(Lazy(Left(a.!).of[a, f[FreeT[f, n, a]]])))
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = FreeT {
            import nM.`for`
            for {
                a <- run(m)
            } {
                a match {
                    case Left(x) => run { f(x) }
                    case Right(xc) => nM.`return`(Right {
                        import fF.`for`
                        for { p <- xc } yield op_>>=(p)(f)
                    })
                }
            }
        }
        // MonadFree
        override val functor: Functor[f] = fF
        override def free[a, b](m: m[a]): m[Either[a, f[m[a]]]] = _asMonadTrans[f].lift(run(m))
        override def wrap[a](f: f[m[a]]): m[a] = FreeT(nM.`return`(Right(f)))
    }
}
