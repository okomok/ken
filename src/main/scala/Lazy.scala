

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


case class Lazy[+R](_1: () => R) extends (() => R) {
    private[this] lazy val v = _1()
    override def apply() = v
}

object Lazy {
    def apply[R](body: => R)(implicit i: DummyImplicit) = new Lazy(() => body)
    //implicit def _fromExpr[R](from: => R): Lazy[R] = apply(from)

    def r[A, B, C](f: A => B => C)(x: A)(y: Lazy[B]): C = f(x)(y())
}
