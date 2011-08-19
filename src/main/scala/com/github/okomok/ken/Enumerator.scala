

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Draft


object Enumerator {
    sealed abstract class Stream[+a]
    final case class Chunks[+a](_1: List[a]) extends Stream[a]
    final case object EOF extends Stream[Nothing]

    object Stream extends Monad[Stream] with ThisIsInstance {
        // Overrides
        //
        // Monad
        private[this] type m[+a] = Stream[a]
        override def `return`[a](a: Lazy[a]): m[a] = Chunks(List.`return`(a))
        override def op_>>=[a, b](m: m[a])(f: a => m[b]): m[b] = m match {
            case Chunks(xs) => _asMonoid[b].mconcat(List.fmap(f)(xs))
            case EOF => EOF
        }

        // Instances
        //
        implicit def _asMonoid[a]: Monoid[Stream[a]] = new Monoid[Stream[a]] {
            private[this] type m = Stream[a]
            val i = Monoid[List[a]]
            override def mempty: m = Chunks(i.mempty)
            override def mappend: m => Lazy[m] => m = x => y => (x, y.!) match {
                case (Chunks(xs), Chunks(ys)) => Chunks(i.mappend(xs)(ys))
                case _ => EOF
            }
        }
    }
}

private[ken] final class _Enumerators[n[+_]](val inner: Monad[n]) {
    private[this] implicit def innerForComp[a](x: n[a]): inner.ForComp[a] = inner.forComp(x)
    private[this] implicit def innerOp_>>=[a](x: n[a]): inner.Op_>>=[a] = inner.>>=(x)

    sealed abstract class Step[-a, +b]
    final case class Continue[a, b](_1: Stream[a] => Iteratee[a, b]) extends Step[a, b]
    final case class Yield[a, b](_1: b, _2: Stream[a]) extends Step[a, b]
    final case class Error(_1: Throwable) extends Step[Any, Nothing]

    final case class Iteratee[-a, +b](override val get: n[Step[a, b]]) extends NewtypeOf[n[Step[a, b]]] with Kind.alwaysThis

    object Iteratee extends Kind.AbstractNewtype2 {
        override type apply2[-a, +b] = Iteratee[a, b]
        override type oldtype2[-a, +b] = n[Step[a, b]]

        def run[a, b](n: Iteratee[a, b]): n[Step[a, b]] = n.run

        def map[m[+_], a, a_, b, b_](f: n[Step[a, b]] => m[Step[a_, b_]])(n: Iteratee[a, b]): NewtypeOf[m[Step[a_, b_]]] = NewtypeOf { f(run(n)) }

        implicit def dependent[a, b](n: NewtypeOf[n[Step[a, b]]]): Iteratee[a, b] = Iteratee { n.run }
    }
}
