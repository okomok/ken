

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


sealed trait Step[-a, +n[+_], +b] extends Up[Step[a, n, b]]

final case class Yield[a, b](x: b, extra: Stream[a]) extends Step[a, Kind.Nothing1, b]
final case class Error(err: SomeException) extends Step[Any, Kind.Nothing1, Nothing]

// java.lang.Error: typeConstructor inapplicable for <none>
// final case class Continue[a, n[+_], b](override val k: Stream[a] => Iteratee[a, n, b]) extends Step[a, n, b]

private[enumerator] sealed trait ContinueTag
final class Continue[a, n[+_], b](private val k: Stream[a] => Iteratee[a, n, b]) extends Step[a, n, b] with ContinueTag

object Continue {
    def apply[a, n[+_], b](k: Stream[a] => Iteratee[a, n, b]): Step[a, n, b] = new Continue(k)
    def unapply[a, n[+_], b](s: Step[a, n, b]): Option[Stream[a] => Iteratee[a, n, b]] = {
        if (s.isInstanceOf[ContinueTag]) Some(s.asInstanceOf[Continue[a, n, b]].k)
        else None
    }
}
