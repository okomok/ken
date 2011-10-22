

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2008-2011 Edward Kmett
// Copyright 2004-2008 Dave Menendez
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


/* TODO

trait Comonad[w[+_]] extends Functor[w] { outer =>
    // Prefer `apply` to `w` for type-parameter inference.
    final val asComonad: Comonad[apply] = this

}


trait ComonadProxy[w[+_]] extends Comonad[w] {
    def selfComonad: Comonad[w]
}


object Comonad extends ComonadInstance {
    def apply[w <: Kind.Function1](implicit i: Comonad[w#apply]): Comonad[w#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: Comonad[nt#oldtype1]): Comonad[nt#apply] = new Comonad[nt#apply] {
        private type w[+a] = nt#apply[a]
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: Comonad[nt#apply]): Comonad[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


trait ComonadInstance { this: Comonad.type
}

*/
