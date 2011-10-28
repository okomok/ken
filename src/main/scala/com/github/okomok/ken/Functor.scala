

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Functor[f[+_]] extends Typeclass1[f] { outer =>
    // Prefer `apply` to `f` for type-parameter inference.
    final val asFunctor: Functor[apply1] = this

    // Core
    //
    def fmap[a, b](x: a => b): f[a] => f[b]

    // Extra
    //
    @Annotation.aliasOf("fmap")
    final def op_<@>[a, b](x: a => b): f[a] => f[b] = fmap(x)

    def op_<@[a, b](x: Lazy[a])(y: f[b]): f[a] = fmap[b, a](_ => x)(y)

    // Operators
    //
    private[ken] sealed class Op_<@>[a, b](x: a => b) {
        def <@>(y: f[a]): f[b] = op_<@>(x)(y)
    }
    final implicit def <@>[a, b](x: a => b): Op_<@>[a, b] = new Op_<@>(x)

    private[ken] sealed class Op_<@[a](x: Lazy[a]) {
        def <@[b](y: f[b]): f[a] = op_<@(x)(y)
    }
    final implicit def <@[a](x: a): Op_<@[a] = new Op_<@(x)

    // For-comprehension
    //
    type For[+a] = ken.For[f, a]
    type ForProxy[+a] = ken.ForProxy[f, a]

    @Annotation.compilerWorkaround("2.9.1", 5070)
    implicit def `for`[a](x: f[a]): ken.For[f, a] = new For[a] {
        override def map[b](g: a => b): f[b] = outer.fmap(g)(x)
    }
}


trait FunctorProxy[f[+_]] extends Functor[f] {
    def selfFunctor: Functor[f]

    override def fmap[a, b](x: a => b): f[a] => f[b] = selfFunctor.fmap(x)
    override def op_<@[a, b](x: Lazy[a])(y: f[b]): f[a] = selfFunctor.op_<@(x)(y)

    override implicit def `for`[a](x: f[a]): ken.For[f, a] = selfFunctor.`for`(x)
}


object Functor extends FunctorInstance {
    def apply[f <: Kind.Function1](implicit i: Functor[f#apply1]): Functor[f#apply1] = i

    def deriving[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Functor[nt#oldtype1]): Functor[nt#apply1] = new Functor[nt#apply1] {
        private type f[+a] = nt#apply1[a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = m => j.newOf { i.fmap(f)(j.oldOf(m)) }
    }

    def weak[nt <: Kind.Newtype1](implicit j: Newtype1[nt#apply1, nt#oldtype1], i: Functor[nt#apply1]): Functor[nt#oldtype1] = deriving[Kind.coNewtype1[nt]](j.coNewtype, i)
}


sealed trait FunctorInstance { this: Functor.type =>
    implicit val _ofWeakIdentity: MonadFix[WeakIdentity.apply] with Comonad[WeakIdentity.apply]= WeakIdentity
    implicit def _ofTuple2[z]: Comonad[Tuple2.apply[z]#apply1] = Tuple2._asComonad[z]
    implicit def _ofFunction[z]: MonadReader[z, Function.apply[z]#apply1] = Function._asMonadReader[z]

    implicit def ofScalaTraversable[CC[+X] <: scala.collection.GenTraversableLike[X, CC[X]]](implicit mf: Scala.CanMapFrom[CC]): MonadPlus[CC] = Scala.Traversable._asMonadPlus(mf)
    implicit val ofScalaOption: MonadPlus[Option] = Scala.Option
}
