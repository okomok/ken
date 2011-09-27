

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadCont[m[+_]] extends Monad[m] {
    final val asMonadCont: MonadCont[apply] = this

    // Core
    //
    def callCC[a, b](f: (a => m[b]) => m[a]): m[a]
}


trait MonadContProxy[m[+_]] extends MonadCont[m] with MonadProxy[m] {
    def selfMonadCont: MonadCont[m]
    override def selfMonad: Monad[m] = selfMonadCont

    override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = selfMonadCont.callCC(f)
}


object MonadCont extends MonadContInstance {
    def apply[m <: Kind.Function1](implicit i: MonadCont[m#apply]): MonadCont[m#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadCont[nt#oldtype1]): MonadCont[nt#apply] = new MonadCont[nt#apply] with MonadProxy[nt#apply] {
        private type m[+a] = nt#apply[a]
        override val selfMonad = Monad.deriving[nt]

        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = j.newOf {
            i.callCC { (c: a => nt#oldtype1[b]) =>
                j.oldOf { f( a => j.newOf(c(a)) ) }
            }
        }
    }

    def derivingT[mt <: Kind.MonadT](implicit j: MonadT[mt#apply, mt#innerMonad, mt#baseMonad], i: MonadCont[mt#innerMonad]): MonadCont[mt#apply] = new MonadCont[mt#apply] with MonadProxy[mt#apply] {
        private type m[+a] = mt#apply[a]
        private type n[+a] = mt#innerMonad[a]
        private type u[+a] = mt#baseMonad[a]
        override def selfMonad = j

        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = j.newOf {
            i.callCC { (c: u[a] => n[u[b]]) =>
                j.oldOf { f( a => j.newOf { c(j.baseMonad.`return`(a)) } ) }
            }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: MonadCont[nt#apply]): MonadCont[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadContInstance { this: MonadCont.type =>
    implicit def ofMonadT[m[+_], n[+_], u[+_]](implicit j: MonadT[m, n, u], i: MonadCont[n]): MonadCont[m] = derivingT[MonadT[m, n, u]]
}
