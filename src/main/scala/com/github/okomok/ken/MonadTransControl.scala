

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// n: inner
// m: transformer (MaybeT etc)
// u: base (Maybe etc)
//
trait MonadTransControl[n[+_], m[+_], u[+_]] extends MonadTrans[n, m] with Kind.AbstractMonadTransControl {
    override type baseResult[+a] = u[a]
    type Run = MonadTransControl.Run[n, m, u]

    final val asMonadTransControl: MonadTransControl[innerMonad, apply, baseResult] = this

    // Core
    //
    def liftControl[a](f: Run => n[a]): m[a]

    // Extra
    //
    def control[a](f: Run => n[m[a]])(implicit j: Monad[m]): m[a] = j.join(liftControl(f))
}


trait MonadTransControlProxy[n[+_], m[+_], u[+_]] extends MonadTransControl[n, m, u] with MonadTransProxy[n, m] {
    def selfMonadTransControl: MonadTransControl[n, m, u]
    override def selfMonadTrans: MonadTrans[n, m] = selfMonadTransControl

    override def liftControl[a](f: Run => n[a]): m[a] = selfMonadTransControl.liftControl(f)
    override def control[a](f: Run => n[m[a]])(implicit j: Monad[m]): m[a] = selfMonadTransControl.control(f)(j)
}


object MonadTransControl {
    def apply[m <: Kind.MonadTransControl](implicit i: MonadTransControl[m#innerMonad, m#apply, m#baseResult]): MonadTransControl[m#innerMonad, m#apply, m#baseResult] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTransControl](implicit i: MonadTransControl[ot#innerMonad, ot#apply, ot#baseResult], j: Newtype1[nt#apply, ot#apply]): MonadTransControl[ot#innerMonad, nt#apply, ot#baseResult] = new MonadTransControl[ot#innerMonad, nt#apply, ot#baseResult] with MonadTransProxy[ot#innerMonad, nt#apply] {
        type n[+a] = ot#innerMonad[a]
        type m[+a] = nt#apply[a]
        type u[+a] = ot#baseResult[a]
        override val selfMonadTrans = MonadTrans.deriving[nt, ot](i, j)
        override def liftControl[a](f: Run => n[a]): m[a] = j.newOf {
            i.liftControl { run =>
                f {
                    new Run {
                        override def apply[o[+_], b](t: m[b])(implicit mo: Monad[o]): n[o[u[b]]] = run[o, b](j.oldOf(t))
                    }
                }
            }
        }
    }

    def weak[nt <: Kind.MonadTransControl](implicit i: MonadTransControl[nt#innerMonad, nt#apply, nt#baseResult], j: Newtype1[nt#apply, nt#oldtype1]): MonadTransControl[nt#innerMonad, nt#oldtype1, nt#baseResult] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)

    // Run types
    //
    trait Run[n[+_], m[+_], u[+_]] {
        def apply[o[+_], b](t: m[b])(implicit i: Monad[o]): n[o[u[b]]]
    }

    trait RunInBase[m[+_], base[+_]] {
        def apply[b](t: m[b]): base[m[b]]
    }

    // Lifting
    //
    def idLiftControl[m[+_], a](f: RunInBase[m, m] => m[a])(implicit j: Monad[m]): m[a] = f {
        new RunInBase[m, m] {
            override def apply[b](t: m[b]): m[m[b]] = j.liftM[b, m[b]](j.`return`[b])(t)
        }
    }
}
