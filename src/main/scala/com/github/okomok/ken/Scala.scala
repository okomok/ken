

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder


object Scala {
    // See: Scalaz' CanBuildAnySelf
    trait CanMapFrom[CC[+_]] {
        def apply[B]: Builder[B, CC[B]] = canBuild[B, B].apply()
        def canBuild[A, B]: CanBuildFrom[CC[A], B, CC[B]]
    }

    object CanMapFrom {
        type _Impl[CC[+_]] = CanBuildFrom[CC[A], B, CC[B]] forSome { type A; type B }

        implicit def _fromImpl[CC[+_]](implicit bf: _Impl[CC]): CanMapFrom[CC] = new CanMapFrom[CC] {
            override def canBuild[A, B] = bf.asInstanceOf[CanBuildFrom[CC[A], B, CC[B]]]
        }

        implicit def _toCanBuildFrom[CC[+_], A, B](mf: CanMapFrom[CC]): CanBuildFrom[CC[A], B, CC[B]] = mf.canBuild[A, B]
    }

    object Traversable {
        private[ken] def _asMonadPlus[CC[+X] <: GenTraversableLike[X, CC[X]]](implicit mf: CanMapFrom[CC]): MonadPlus[CC] = new MonadPlus[CC] {
            private type m[+a] = CC[a]
            // Monad
            override def `return`[a](x: Lazy[a]): m[a] = {
                val b = mf.apply[a]
                b += x
                b.result
            }
            override def op_>>=[a, b](x: m[a])(f: a => m[b]): m[b] = x.flatMap(f)(mf)
            // MonadPlus
            override def mzero: m[Nothing] = mf.apply[Nothing].result
            override def mplus[a](x: m[a])(y: Lazy[m[a]]): m[a] = x.++(y)(mf)
        }

        private[ken] def _asTraversable[CC[+X] <: GenTraversableLike[X, CC[X]]](implicit mf: CanMapFrom[CC]): Traversable[CC] = new Traversable[CC] with FunctorProxy[CC] {
            private type t[+a] = CC[a]
            override val selfFunctor: selfFunctor = _asMonadPlus[CC](mf)
            override def foldr[a, b](f: a => Lazy[b] => b)(z: b)(t: t[a]): b = t.foldRight(z)((a, b) => f(a)(b))
            override def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = t.foldLeft(z)((a, b) => f(a)(b))
            override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = {
                import i.{<@>, <*>}
                def cons_f(x: a)(ys: Lazy[f[List[b]]]): f[List[b]] = Function.from(List.op_!::[b]) <@> f(x) <*> ys
                val tmp: f[List[b]] = foldr(cons_f)(i.pure(Nil))(t)
                ((xs: List[b]) => {
                    val b = mf.apply[b]
                    List.foreach((x: b) => b += x)(xs)
                    b.result
                }) <@> tmp
            }
        }
    }

    object Option extends MonadPlus[Option] with Traversable[Option] {
        // Functor
        private type f[+a] = Option[a]
        override def fmap[a, b](f: a => b): f[a] => f[b] = {
            case None => None
            case Some(a) => Some(f(a))
        }
        // Monad
        private type m[+a] = f[a]
        override def `return`[a](x: Lazy[a]): m[a] = Some(x)
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = m match {
            case Some(x) => k(x)
            case None => None
        }
        override def op_>>[b](m: m[_])(k: Lazy[m[b]]): m[b] = m match {
            case Some(_) => k
            case None => None
        }
        // MonadPlus
        override def mzero: m[Nothing] = None
        override def mplus[a](xs: m[a])(ys: Lazy[m[a]]): m[a] = xs match {
            case None => ys
            case _ => xs
        }
        // Foldable
        private type t[+a] = Option[a]
        override def foldr[a, b](f: a => Lazy[b] => b)(z: b)(t: t[a]): b = t match {
            case None => z
            case Some(x) => f(x)(z)
        }
        override def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = t match {
            case None => z
            case Some(x) => f(z)(x)
        }
        // Traversable
        override def traverse[f[+_], a, b](f: a => f[b])(t: t[a])(implicit i: Applicative[f]): f[t[b]] = t match {
            case None => i.pure(None)
            case Some(x) => i.op_<@>(id[b => Option[b]](Some(_)))(f(x))
        }
    }
}
