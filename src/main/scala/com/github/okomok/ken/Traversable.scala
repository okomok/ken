

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Traversable[t[+_]] extends Functor[t] with Foldable[t] {
    final val asTraversable: Traversable[apply] = this

    // Core
    //
    def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = sequenceA(fmap(f)(t))
    def sequenceA[f[+_], a](t: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = traverse(id[f[a]])(t)(i)
    def mapM[m[+_], a, b](f: a => m[b])(t: t[a])(implicit i: Monad[m]): m[t[b]] = traverse(f)(t)
    def sequence[m[+_], a](t: t[m[a]])(implicit i: Monad[m]): m[t[a]] = mapM(id[m[a]])(t)(i)

    // Extra
    //
    def `for`[f[+_], a, b](t: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[t[b]] = traverse(f)(t)
    def forM[m[+_], a, b](t: t[a])(f: a => m[b])(implicit i: Monad[m]): m[t[b]] = mapM(f)(t)

    def mapAccumL[a, b, c](f: a => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = {
        implicit  val j = Applicative[StateL.apply[a]]
        traverse( (x: b) => j.infer( StateL(flip(f)(x)) ) )(t).get.apply(s)
    }

    def mapAccumR[a, b, c](f: (=> a) => b => (a, c))(s: => a)(t: t[b]): (a, t[c]) = {
        implicit  val j = Applicative[StateR.apply[a]]
        traverse( (x: b) => j.infer( StateR(flip(f)(x)) ) )(t).get.apply(s)
    }

    def fmapDefault[a, b](f: a => b)(t: t[a]): t[b] = traverse(f andThen Identity.apply)(t).get

    def foldMapDefault[m, a](f: a => m)(t: t[a])(implicit i: Monoid[m]): m = {
        implicit val j = Applicative[Const.apply[m]]
        traverse( (x: a) => j.infer( Const(f(x)) ) )(t).get
    }
}


trait TraversableProxy[t[+_]] extends Traversable[t] with FunctorProxy[t] with FoldableProxy[t] {
    override def self: Traversable[t]

    override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = self.traverse(f)(t)(i)
    override def sequenceA[f[+_], a](t: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = self.sequenceA(t)(i)
    override def mapM[m[+_], a, b](f: a => m[b])(t: t[a])(implicit i: Monad[m]): m[t[b]] = self.mapM(f)(t)(i)
    override def sequence[m[+_], a](t: t[m[a]])(implicit i: Monad[m]): m[t[a]] = self.sequence(t)(i)

    override def `for`[f[+_], a, b](t: t[a])(f: a => f[b])(implicit i: Applicative[f]): f[t[b]] = self.`for`(t)(f)(i)
    override def forM[m[+_], a, b](t: t[a])(f: a => m[b])(implicit i: Monad[m]): m[t[b]] = self.forM(t)(f)(i)
    override def mapAccumL[a, b, c](f: a => b => (a, c))(s: a)(t: t[b]): (a, t[c]) = self.mapAccumL(f)(s)(t)
    override def mapAccumR[a, b, c](f: (=> a) => b => (a, c))(s: => a)(t: t[b]): (a, t[c]) = self.mapAccumR(f)(s)(t)
    override def fmapDefault[a, b](f: a => b)(t: t[a]): t[b] = self.fmapDefault(f)(t)
    override def foldMapDefault[m, a](f: a => m)(t: t[a])(implicit i: Monoid[m]): m = self.foldMapDefault(f)(t)(i)
}


object Traversable {
    def apply[t <: Kind.Function1](implicit i: Traversable[t#apply]): Traversable[t#apply] = i
}


// StateL
//
private[ken] final case class StateL[s, +a](override val get: s => (s, a)) extends Strong[s => (s, a)]

private[ken] object StateL extends Kind.Function {
    sealed trait apply[s] extends Kind.Strong1 {
        override type apply[+a] = StateL[s, a]
        override type weak[+a] = s => (a, s)
    }

    implicit def _asApplicative[s]: Applicative[({type f[+a] = StateL[s, a]})#f] = new Applicative[({type f[+a] = StateL[s, a]})#f] {
        // Functor
        private[this] type f[+a] = StateL[s, a]
        override def fmap[a, b](f: a => b)(k: f[a]): f[b] = StateL { s =>
            val (s_, v) = k.get.apply(s)
            (s_, f(v))
        }
        // Applicative
        override def `pure`[a](x: => a): f[a] = StateL { s => (s, x) }
        override def op_<*>[a, b](kf: f[a => b])(kv: f[a]): f[b] = StateL { s =>
            val (s_, f) = kf.get.apply(s)
            val (s__, v) = kv.get.apply(s_)
            (s__, f(v))
        }
    }
}


// StateR
//
private[ken] final case class StateR[s, +a](override val get: (=> s) => (s, a)) extends Strong[(=> s) => (s, a)]

private[ken] object StateR extends Kind.Function {
    sealed trait apply[s] extends Kind.Strong1 {
        override type apply[+a] = StateR[s, a]
        override type weak[+a] = (=> s) => (a, s)
    }

    implicit def _asApplicative[s]: Applicative[({type f[+a] = StateR[s, a]})#f] = new Applicative[({type f[+a] = StateR[s, a]})#f] {
        // Functor
        private[this] type f[+a] = StateR[s, a]
        override def fmap[a, b](f: a => b)(k: f[a]): f[b] = StateR { s =>
            val (s_, v) = k.get.apply(s)
            (s_, f(v))
        }
        // Applicative
        override def `pure`[a](x: => a): f[a] = StateR { s => (s, x) }
        override def op_<*>[a, b](kf: f[a => b])(kv: f[a]): f[b] = StateR { s =>
            val (s_, v) = kv.get.apply(s)
            val (s__, f) = kf.get.apply(s_)
            (s__, f(v))
        }
    }
}
