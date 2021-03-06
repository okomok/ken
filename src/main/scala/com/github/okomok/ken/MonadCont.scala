

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait MonadCont[m[+_]] extends Monad[m] {
    final val asMonadCont: MonadCont[apply1] = this

    // Core
    //
    def callCC[a, b](f: (a => m[b]) => m[a]): m[a]
}


trait MonadContProxy[m[+_]] extends MonadCont[m] with MonadProxy[m] {
    type selfMonadCont = MonadCont[m]
    def selfMonadCont: selfMonadCont
    override def selfMonad: selfMonad = selfMonadCont

    override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = selfMonadCont.callCC(f)
}


object MonadCont extends MonadContInstance {
    def apply[m <: Kind.Function1](implicit i: MonadCont[m#apply1]): MonadCont[m#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadCont[nt#oldtype1]): MonadCont[nt#apply1] = new MonadCont[nt#apply1] with MonadProxy[nt#apply1] {
        private type m[+a] = nt#apply1[a]
        override val selfMonad: selfMonad = Monad.deriving[nt]

        override def callCC[a, b](f: (a => m[b]) => m[a]): m[a] = j.newOf {
            i.callCC { (c: a => nt#oldtype1[b]) =>
                j.oldOf { f( a => j.newOf(c(a)) ) }
            }
        }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: MonadCont[nt#apply1]): MonadCont[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait MonadContInstance { this: MonadCont.type =>
}
