

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2008-2011 Edward Kmett
// Copyright 2004-2008 Dave Menendez
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Comonad[w[+_]] extends Extend[w] {
    final val asComonad: Comonad[apply] = this

    // Core
    //
    def extract[a](w: w[a]): a

    // Extra
    //
    def liftW[a, b](f: a => b): w[a] => w[b] = extend((w: w[a]) => f(extract(w)))
    def wfix[a](w: w[w[a] => a]): a = extract(w)(extend(wfix[a])(w))
}


trait ComonadProxy[w[+_]] extends Comonad[w] with ExtendProxy[w] {
    def selfComonad: Comonad[w]
    override def selfExtend: Extend[w] = selfComonad

    override def extract[a](w: w[a]): a = selfComonad.extract(w)

    override def liftW[a, b](f: a => b): w[a] => w[b] = selfComonad.liftW(f)
    override def wfix[a](w: w[w[a] => a]): a = selfComonad.wfix(w)
}


object Comonad extends ComonadInstance {
    def apply[w <: Kind.Function1](implicit i: Comonad[w#apply]): Comonad[w#apply] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: Comonad[nt#oldtype1]): Comonad[nt#apply] = new Comonad[nt#apply] with ExtendProxy[nt#apply] {
        private type w[+a] = nt#apply[a]
        override val selfExtend = Extend.deriving[nt]
        override def extract[a](w: w[a]): a = i.extract(j.oldOf(w))
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply, nt#oldtype1], i: Comonad[nt#apply]): Comonad[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait ComonadInstance { this: Comonad.type =>
    implicit def ofFunction[m](implicit i: Monoid[m]): Comonad[Function.apply[m]#apply] = Function._asComonad(i)
}
