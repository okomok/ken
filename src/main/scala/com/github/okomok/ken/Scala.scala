

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


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
    }
}
