

// Copyright Shunsuke Sogame 2011.
//
// https://github.com/pepeiborra/control-monad-free
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Codensity[n[+_], +a] {
    def apply[r](h: a => n[r]): n[r]

    final def improve(implicit nM: Monad[n]): n[a] = apply(nM.`return`[a])
}


object Codensity extends CodensityAs with MonadTrans[Codensity] with Kind.FunctionLike with ThisIsInstance {
    trait apply[n[+_]] extends apply1[n]
    trait apply1[n[+_]] extends Kind.Function1 {
        override type apply1[+a] = Codensity[n, a]
    }

    def rep[n[+_], a](n: n[a])(implicit nM: Monad[n]): Codensity[n, a] = new Codensity[n, a] {
        override def apply[r](h: a => n[r]): n[r] = nM.op_>>=(n)(h)
    }

    // Overrides
    //
    // MonadTrans
    private type t[n[+_], a] = Codensity[n, a]
    override def lift[n[+_], a](n: n[a])(implicit nM: Monad[n]): t[n, a] = new Codensity[n, a] {
        override def apply[r](h: a => n[r]): n[r] = nM.op_>>=(n)(h)
    }
}

private[ken] sealed trait CodensityAs0 { this: Codensity.type =>
    implicit def _asFunctor[n[+_]]: Functor[apply1[n]#apply1] = new Functor[apply1[n]#apply1] {
        private type f[+a] = Codensity[n, a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => new Codensity[n, b] {
            override def apply[r](h: b => n[r]): n[r] = m(h `.` f)
        }
    }
}

private[ken] sealed trait CodensityAs1 extends CodensityAs0 { this: Codensity.type =>
    implicit def _asMonad[n[+_]]: Monad[apply1[n]#apply1] = new Monad[apply1[n]#apply1] with FunctorProxy[apply1[n]#apply1] {
        private type m[+a] = Codensity[n, a]
        override val selfFunctor = _asFunctor[n]
        override def `return`[a](x: Lazy[a]): m[a] = new Codensity[n, a] {
            override def apply[r](h: a => n[r]): n[r] = h(x)
        }
        override def op_>>=[a, b](p: m[a])(k: a => m[b]): m[b] = new Codensity[n, b] {
            override def apply[r](h: b => n[r]): n[r] = p(a => k(a)(h))
        }
    }
}

private[ken] sealed trait CodensityAs2 extends CodensityAs1 { this: Codensity.type =>
    implicit def _asMonadPlus[n[+_]](implicit nM: MonadPlus[n]): MonadPlus[apply1[n]#apply1] = new MonadPlus[apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = Codensity[n, a]
        override val selfMonad = _asMonad[n]
        override val mzero: m[Nothing] = rep(nM.mzero)
        override def mplus[a](p1: m[a])(p2: Lazy[m[a]]): m[a] = rep(nM.mplus(p1.improve)(p2.improve))
    }
}

private[ken] sealed trait CodensityAs extends CodensityAs2 { this: Codensity.type =>
    implicit def _asMonadFree[f[+_], n[+_]](implicit fF: Functor[f], nM: MonadFree[f, n]): MonadFree[f, apply1[n]#apply1] = new MonadFree[f, apply1[n]#apply1] with MonadProxy[apply1[n]#apply1] {
        private type m[+a] = Codensity[n, a]
        override val selfMonad = _asMonad[n]
        override val functor: Functor[f] = fF
        override def free[a, b](m: m[a]): m[Either[a, f[m[a]]]] = {
            val rep_ : n[a] => m[a] = rep
            rep(nM.fmap(Functor[Either.apply1[a]].fmap(fF.fmap(rep_)))( nM.free(m.improve(nM)) ))
        }
        override def wrap[a](t: f[m[a]]): m[a] = new Codensity[n, a] {
            override def apply[r](h: a => n[r]): n[r] = nM.wrap {
                import fF.`for`
                for { p <- t } yield p(h)
            }
        }
    }
}
