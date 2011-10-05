

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


sealed trait Step[-a, +n[+_], +b] extends Up[Step[a, n, b]]

sealed trait ContinueTag


//final case class Continue[a, n[+_], b](override val k: Stream[a] => Iteratee[a, n, b]) extends Step[a, n, b]
@Annotation.compilerWorkaround("2.9.1") // java.lang.Error: typeConstructor inapplicable for <none>
final class Continue[a, n[+_], b](private val k: Stream[a] => Iteratee[a, n, b]) extends Step[a, n, b] with ContinueTag

object Continue {
    def apply[a, n[+_], b](k: Stream[a] => Iteratee[a, n, b]): Step[a, n, b] = new Continue(k)
    def unapply[a, n[+_], b](x: Step[a, n, b]): Option[Stream[a] => Iteratee[a, n, b]] = {
        if (x.isInstanceOf[ContinueTag]) Some(x.asInstanceOf[Continue[a, ({type n[+x] = Nothing})#n, b]].k)
        else None
    }
}

final case class Yield[a, b](x: b, extra: Stream[a]) extends Step[a, ({type n[+x] = Nothing})#n, b]
final case class Error(err: SomeException) extends Step[Any, ({type n[+x] = Nothing})#n, Nothing]

