

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Pointed[f[+_]] extends Functor[f] {
    final val asPointed: Pointed[apply1] = this

    // Core
    //
    def pure[a](x: Lazy[a]): f[a]
}


trait PointedProxy[f[+_]] extends Pointed[f] with FunctorProxy[f] {
    type selfPointed = Pointed[f]
    def selfPointed: selfPointed
    override def selfFunctor: selfFunctor = selfPointed

    override def pure[a](x: Lazy[a]): f[a] = selfPointed.pure(x)
}


object Pointed extends PointedInstance {
    def apply[f <: Kind.Function1](implicit i: Pointed[f#apply1]): Pointed[f#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Pointed[nt#oldtype1]): Pointed[nt#apply1] = new Pointed[nt#apply1] with FunctorProxy[nt#apply1] {
        private type f[+a] = nt#apply1[a]
        override val selfFunctor: selfFunctor = Functor.deriving[nt]

        override def pure[a](x: Lazy[a]): f[a] = j.newOf { i.pure(x) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Pointed[nt#apply1]): Pointed[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait PointedInstance { this: Pointed.type =>
    implicit def _ofTuple2[z](implicit ma: Monoid[z]): Applicative[Tuple2.apply[z]#apply1] = Tuple2._asApplicative(ma)
}
