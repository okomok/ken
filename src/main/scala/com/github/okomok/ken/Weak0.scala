

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Weakly-typed instances
 */
trait Weak0[p, d] extends Klass0[d] {
    final def asWeak0: Weak0[p, apply] = this

    // Core
    //
    def wrap(d: => d): p
    def unwrap(p: p): d

    // Instances
    //
    implicit def monoid(implicit i: Monoid[p]): Monoid[d] = new Monoid[d] {
        private[this] type m = d
        override val mempty: m = unwrap(i.mempty)
        override val mappend: m => (=> m) => m = x => y => unwrap(i.mappend(wrap(x))(wrap(y)))
        override val mconcat: List[m] => m = { xs => unwrap(i.mconcat(List.map[m, p](x => wrap(x))(xs))) }
    }
}


trait Weak0Proxy[p, d] extends Weak0[p, d] with Proxy {
    override def self: Weak0[p, d]

    override def wrap(d: => d): p = self.wrap(d)
    override def unwrap(p: p): d = self.unwrap(p)

    override def monoid(implicit i: Monoid[p]): Monoid[d] = self.monoid
}


object Weak0 {
    def apply[p, d](implicit i: Weak0[p, d]): Weak0[p, d] = i
}
