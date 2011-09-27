

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Pending...


// m: transformer (MaybeT etc)
// n: inner
// u: base (Maybe etc)
//
trait MonadTransControl[m[+_], n[+_], u[+_]] extends MonadTrans[m, n] with Kind.MonadTransControl {
    override type baseResult[+a] = u[a]
    type Run = MonadTransControl.Run[n, m, u]

    final val asMonadTransControl: MonadTransControl[apply, innerMonad, baseResult] = this

    // Core
    //
    def liftControl[a](f: Run => n[a]): m[a]

    // Extra
    //
    def control[a](f: Run => n[m[a]])(implicit j: Monad[m]): m[a] = j.join(liftControl(f))
}


trait MonadTransControlProxy[m[+_], n[+_], u[+_]] extends MonadTransControl[m, n, u] with MonadTransProxy[m, n] {
    def selfMonadTransControl: MonadTransControl[m, n, u]
    override def selfMonadTrans: MonadTrans[m, n] = selfMonadTransControl

    override def liftControl[a](f: Run => n[a]): m[a] = selfMonadTransControl.liftControl(f)
    override def control[a](f: Run => n[m[a]])(implicit j: Monad[m]): m[a] = selfMonadTransControl.control(f)(j)
}


object MonadTransControl {
    def apply[m <: Kind.MonadTransControl](implicit i: MonadTransControl[m#apply, m#innerMonad, m#baseResult]): MonadTransControl[m#apply, m#innerMonad, m#baseResult] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.MonadTransControl](implicit j: Newtype1[nt#apply, ot#apply], i: MonadTransControl[ot#apply, ot#innerMonad, ot#baseResult]): MonadTransControl[nt#apply, ot#innerMonad, ot#baseResult] = new MonadTransControl[nt#apply, ot#innerMonad, ot#baseResult] with MonadTransProxy[nt#apply, ot#innerMonad] {
        type n[+a] = ot#innerMonad[a]
        type m[+a] = nt#apply[a]
        type u[+a] = ot#baseResult[a]
        override val selfMonadTrans = MonadTrans.deriving[nt, ot]

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

    def weak[nt <: Kind.MonadTransControl](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadTransControl[nt#apply, nt#innerMonad, nt#baseResult]): MonadTransControl[nt#oldtype1, nt#innerMonad, nt#baseResult] = deriving[Kind.quote1[nt#oldtype1], nt](j.coNewtype, i)

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
