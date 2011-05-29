

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[a] {
    def mempty: a
    def mappend(x: a)(y: a): a
    def mconcat(x: List[a]): a = x.foldRight(mempty)(mappend(_)(_))

    private[ken] class _Mappend_(x: a) {
        def _mappend_(y: a): a = mappend(x)(y)
    }
    implicit def _mappend_(x: a): _Mappend_ = new _Mappend_(x)
}


object Monoid {

    implicit def List[a]: Monoid[List[a]] = new Monoid[List[a]] {
        override def mempty: List[a] = Nil
        override def mappend(x: List[a])(y: List[a]): List[a] = x ::: y
    }

    implicit def Function1[a, b](implicit i: Monoid[b]): Monoid[a => b] = new Monoid[a => b] {
        override def mempty: a => b = _ => i.mempty
        override def mappend(f: a => b)(g: a => b): a => b = x => i.mappend(f(x))(g(x))
    }

    implicit val Unit: Monoid[Unit] = new Monoid[Unit] {
        override def mempty: Unit = ()
        override def mappend(x: Unit)(y: Unit): Unit = ()
        override def mconcat(x: List[Unit]): Unit = ()
    }

}
