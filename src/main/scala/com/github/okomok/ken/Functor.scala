

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Functor[f[+_]] extends Typeclass1[f] { outer =>
    // Prefer `apply` to `f` for type-parameter inference.
    final val asFunctor: Functor[apply] = this

    // Core
    //
    def fmap[a, b](x: a => b)(y: f[a]): f[b]

    // Extra
    //
    def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = fmap(x)(y)
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
}


trait FunctorProxy[f[+_]] extends Functor[f] {
    def selfFunctor: Functor[f]

    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = selfFunctor.fmap(x)(y)
    override def op_<@>[a, b](x: a => b)(y: f[a]): f[b] = selfFunctor.op_<@>(x)(y)
    override def op_<@[a, b](x: Lazy[a])(y: f[b]): f[a] = selfFunctor.op_<@(x)(y)
}


object Functor extends FunctorInstance {
    def apply[f <: Kind.Function1](implicit i: Functor[f#apply]): Functor[f#apply] = i

    def deriving[nt <: Kind.Function1, ot <: Kind.Function1](implicit i: Functor[ot#apply], j: Newtype1[nt#apply, ot#apply]): Functor[nt#apply] = new Functor[nt#apply] {
        private type f[+a] = nt#apply[a]
        override def fmap[a, b](f: a => b)(m: f[a]): f[b] = j.newOf { i.fmap(f)(j.oldOf(m)) }
    }

    def weak[nt <: Kind.Newtype1](implicit i: Functor[nt#apply], j: Newtype1[nt#apply, nt#oldtype1]): Functor[nt#oldtype1] = deriving[Kind.quote1[nt#oldtype1], nt](i, j.dual)
}


trait FunctorInstance { this: Functor.type =>
    implicit val ofWeakIdentity: MonadFix[({type m[+a] = a})#m] = WeakIdentity
    implicit def ofFunction[z]: MonadReader[z, ({type m[+a] = z => a})#m] = Function._asMonadReader[z]
    implicit def ofTuple2[z](implicit ma: Monoid[z]): Applicative[({type f[+a] = (z, a)})#f] = Tuple2._asApplicative(ma)

    implicit def ofScalaTraversable[CC[+X] <: scala.collection.GenTraversableLike[X, CC[X]]](implicit mf: Scala.CanMapFrom[CC]): MonadPlus[CC] = Scala.Traversable._asMonadPlus(mf)
    implicit val ofScalaOption: MonadPlus[Option] = Scala.Option
}
