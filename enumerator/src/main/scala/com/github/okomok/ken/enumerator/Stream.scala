

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


sealed abstract class Stream[+a] extends Up[Stream[a]] {
    final def of[b >: a]: Stream[b] = this
}

final case class Chunks[+a](_1: List[a]) extends Stream[a]
case object EOF extends Stream[Nothing]


object Stream extends Monad[Stream] with ThisIsInstance {
    // Overrides
    //
    // Monad
    private type m[+a] = Stream[a]
    override def `return`[a](a: Lazy[a]): m[a] = Chunks(List.`return`(a))
    override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = m match {
        case Chunks(xs) => _asMonoid[b].mconcat(List.fmap(f)(xs))
        case EOF => EOF
    }

    // Instances
    //
    implicit def _asMonoid[a]: Monoid[Stream[a]] = new Monoid[Stream[a]] {
        private val i = Monoid[List[a]]
        override def mempty: mempty = Chunks(i.mempty)
        override def mappend: mappend = x => y => (x, y.!) match {
            case (Chunks(xs), Chunks(ys)) => Chunks(i.mappend(xs)(ys))
            case _ => EOF
        }
    }
}
