

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder


object Scala {
    trait CanMapFrom[CC[+_]] {
        def canBuild[A, B]: CanBuildFrom[CC[A], B, CC[B]]
        final def apply[B]: Builder[B, CC[B]] = canBuild[B, B].apply()
    }

    object CanMapFrom {
        type MaybeCanMap[CC[+_]] = CanBuildFrom[CC[A], B, CC[B]] forSome { type A; type B }

        implicit def fromMaybeCanMap[CC[+_]](implicit bf: MaybeCanMap[CC]): CanMapFrom[CC] = new CanMapFrom[CC] {
            override def canBuild[A, B] = bf.asInstanceOf[CanBuildFrom[CC[A], B, CC[B]]]
        }

        implicit def toCanBuildFrom[CC[+_], A, B](mf: CanMapFrom[CC]): CanBuildFrom[CC[A], B, CC[B]] = mf.canBuild[A, B]
    }

    sealed trait Traversable[CC[+X] <: GenTraversableLike[X, CC[X]]] extends Kind.Function1 {
        override type apply[+a] = CC[a]
    }

    object Traversable {
        def _monad[CC[+X] <: GenTraversableLike[X, CC[X]]](implicit mf: CanMapFrom[CC]): MonadPlus[CC] = new MonadPlus[CC] {
            private[this] type m[+a] = CC[a]
            // Monad
            override def `return`[a](x: => a): m[a] = {
                val b = mf.apply[a]
                b += x
                b.result
            }
            override def op_>>=[a, b](x: m[a])(f: a => m[b]): m[b] = x.flatMap(f)(mf)
            // MonadPlus
            override def mzero: m[Nothing] = mf.apply[Nothing].result
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = x.++(y)(mf)
        }

        val jjj = _monad[scala.List]
        val kkk = Monad[Kind.quote1[scala.List]]

/*
        private def canBuildFrom[Elem, To](b: => Builder[Elem, To]): CanBuildFrom[Any, Elem, To] = new CanBuildFrom[Any, Elem, To] {
            override def apply(from: Any): Builder[Elem, To] = b
            override def apply(): Builder[Elem, To] = b
        }

        def __monad[CC[+X] <: GenTraversable[X]](cm: GenericCompanion[CC]): MonadPlus[CC] = new MonadPlus[CC] {
            private[this] type m[+a] = CC[a]
            // Monad
            override def `return`[a](x: => a): m[a] = cm(x)
            override def op_>>=[a, b](x: m[a])(f: a => m[b]): m[b] = x.flatMap(f)(canBuildFrom(cm.newBuilder[b]))
            // MonadPlus
            override def mzero: m[Nothing] = cm.empty[Nothing]
            override def mplus[a](x: m[a])(y: => m[a]): m[a] = x.++(y)(canBuildFrom(cm.newBuilder[a]))
        }

        val iii = __monad(scala.List)
*/
    }
}
