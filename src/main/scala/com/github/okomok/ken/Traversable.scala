

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Traversable[t[+_]] extends Functor[t] with Foldable[t] { outer =>
    final val asTraversable: Traversable[apply] = this

    // Core
    //
    def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = sequenceA(fmap(f)(t))
    def sequenceA[f[+_], a](t: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = traverse(id[f[a]])(t)(i)
    def mapM[m[+_], a, b](f: a => m[b])(t: t[a])(implicit i: Monad[m]): m[t[b]] = traverse(f)(t)
    def sequence[m[+_], a](t: t[m[a]])(implicit i: Monad[m]): m[t[a]] = mapM(id[m[a]])(t)(i)

    // Overrides
    //
    // Functor
    private[this] type f[+a] = t[a]
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = {
        traverse( (a: a) => Identity(x(a)) )(y).get
    }

    // Extra
    //
    def `for`[f[+_], a, b](t: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[t[b]] = traverse(f)(t)
    def forM[m[+_], a, b](t: t[a])(f: a => m[b])(implicit i: Monad[m]): m[t[b]] = mapM(f)(t)

    def mapAccumL[a, b, c](f: a => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = {
        //implicit val j = Applicative[StateL.apply[a]]
        //traverse( (x: b) => j.infer( StateL(flip(f)(x)) ) )(t).get.apply(s)
        val k = pull[StateL.apply[a]]
        k.traverse( (x: b) => StateL(flip(f)(x)) )(t).get.apply(s)
    }

    def mapAccumR[a, b, c](f: Lazy[a] => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = {
        implicit val j = Applicative[StateR.apply[a]]
        traverse( (x: b) => j.infer( StateR(flip(f)(x)) ) )(t).get.apply(s)
    }

    def fmapDefault[a, b](f: a => b)(t: t[a]): t[b] = traverse((Identity(_: b)) compose f)(t).get

    def foldMapDefault[m, a](f: a => m)(t: t[a])(implicit i: Monoid[m]): m = {
        implicit val j = Applicative[Const.apply[m]]
        // which is readable?
        // traverse[j.apply, a, Nothing]( (x: a) => Const(f(x)) )(t).get
        traverse( (x: a) => j.infer( Const(f(x)) ) )(t).get
    }

    // Pull
    //
    trait TraversablePull[f_ <: Kind.Function1] extends FoldablePull[f_] {
        final def traverse[a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = outer.traverse(f)(t)(i)
        final def sequenceA[a](t: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = outer.sequenceA(t)(i)
        final def mapM[a, b](f: a => m[b])(t: t[a])(implicit i: Monad[m]): m[t[b]] = outer.mapM(f)(t)(i)
        final def sequence[a](t: t[m[a]])(implicit i: Monad[m]): m[t[a]] = outer.sequence(t)(i)
        final def `for`[a, b](t: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[t[b]] = outer.`for`(t)(f)(i)
        final def forM[a, b](t: t[a])(f: a => m[b])(implicit i: Monad[m]): m[t[b]] = outer.forM(t)(f)
    }
    override def pull[f_ <: Kind.Function1]: TraversablePull[f_] = new TraversablePull[f_]{}
}


trait TraversableProxy[t[+_]] extends Traversable[t] with FunctorProxy[t] with FoldableProxy[t] {
    def selfTraversable: Traversable[t]
    override def selfFunctor: Functor[t] = selfTraversable
    override def selfFoldable: Foldable[t] = selfTraversable

    override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = selfTraversable.traverse(f)(t)(i)
    override def sequenceA[f[+_], a](t: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = selfTraversable.sequenceA(t)(i)
    override def mapM[m[+_], a, b](f: a => m[b])(t: t[a])(implicit i: Monad[m]): m[t[b]] = selfTraversable.mapM(f)(t)(i)
    override def sequence[m[+_], a](t: t[m[a]])(implicit i: Monad[m]): m[t[a]] = selfTraversable.sequence(t)(i)

    override def `for`[f[+_], a, b](t: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[t[b]] = selfTraversable.`for`(t)(f)(i)
    override def forM[m[+_], a, b](t: t[a])(f: a => m[b])(implicit i: Monad[m]): m[t[b]] = selfTraversable.forM(t)(f)(i)
    override def mapAccumL[a, b, c](f: a => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = selfTraversable.mapAccumL(f)(s)(t)
    override def mapAccumR[a, b, c](f: Lazy[a] => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = selfTraversable.mapAccumR(f)(s)(t)
    override def fmapDefault[a, b](f: a => b)(t: t[a]): t[b] = selfTraversable.fmapDefault(f)(t)
    override def foldMapDefault[m, a](f: a => m)(t: t[a])(implicit i: Monoid[m]): m = selfTraversable.foldMapDefault(f)(t)(i)
}


object Traversable {
    def apply[t <: Kind.Function1](implicit i: Traversable[t#apply]): Traversable[t#apply] = i
}


// StateL
//
private[ken] final case class StateL[s, +a](override val get: s => (s, a)) extends NewtypeOf[s => (s, a)]

private[ken] object StateL extends Kind.FunctionLike {
    sealed trait apply[s] extends Kind.AbstractNewtype1 {
        override type apply1[+a] = StateL[s, a]
        override type oldtype1[+a] = s => (a, s)
    }

    implicit def _asApplicative[s]: Applicative[({type f[+a] = StateL[s, a]})#f] = new Applicative[({type f[+a] = StateL[s, a]})#f] {
        // Functor
        private[this] type f[+a] = StateL[s, a]
        override def fmap[a, b](f: a => b)(k: f[a]): f[b] = StateL { s =>
            val (s_, v) = k.get.apply(s)
            (s_, f(v))
        }
        // Applicative
        override def `pure`[a](x: Lazy[a]): f[a] = StateL { s => (s, x) }
        override def op_<*>[a, b](kf: f[a => b])(kv: f[a]): f[b] = StateL { s =>
            val (s_, f) = kf.get.apply(s)
            val (s__, v) = kv.get.apply(s_)
            (s__, f(v))
        }
    }
}


// StateR
//
private[ken] final case class StateR[s, +a](override val get: Lazy[s] => (s, a)) extends NewtypeOf[Lazy[s] => (s, a)]

private[ken] object StateR extends Kind.FunctionLike {
    sealed trait apply[s] extends Kind.AbstractNewtype1 {
        override type apply1[+a] = StateR[s, a]
        override type oldtype1[+a] = Lazy[s] => (a, s)
    }

    implicit def _asApplicative[s]: Applicative[({type f[+a] = StateR[s, a]})#f] = new Applicative[({type f[+a] = StateR[s, a]})#f] {
        // Functor
        private[this] type f[+a] = StateR[s, a]
        override def fmap[a, b](f: a => b)(k: f[a]): f[b] = StateR { s =>
            val (s_, v) = k.get.apply(s)
            (s_, f(v))
        }
        // Applicative
        override def `pure`[a](x: Lazy[a]): f[a] = StateR { s => (s, x) }
        override def op_<*>[a, b](kf: f[a => b])(kv: f[a]): f[b] = StateR { s =>
            val (s_, v) = kv.get.apply(s)
            val (s__, f) = kf.get.apply(s_)
            (s__, f(v))
        }
    }
}
