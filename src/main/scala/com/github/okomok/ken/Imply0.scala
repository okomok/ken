

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Implements `d` instances from `p` instances.
 */
trait Imply0[p, d] extends Typeclass {
    final val asImply0: Imply0[p, d] = this

    // Core
    //
    def imply(p: p): d
    def unimply(d: => d): p

    // Instances
    //
    implicit def asMonoid(implicit i: Monoid[p]): Monoid[d] = new Monoid[d] {
        private[this] type m = d
        override val mempty: m = imply(i.mempty)
        override val mappend: m => (=> m) => m = x => y => imply(i.mappend(unimply(x))(unimply(y)))
        override val mconcat: List[m] => m = { xs => imply(i.mconcat(List.map[m, p](x => unimply(x))(xs))) }
    }
}


trait Imply0Proxy[p, d] extends Imply0[p, d] with Proxy {
    override def self: Imply0[p, d]

    override def imply(p: p): d = self.imply(p)
    override def unimply(d: => d): p = self.unimply(d)

    override def asMonoid(implicit i: Monoid[p]): Monoid[d] = self.asMonoid
}


object Imply0 {
    def apply[p, d](implicit i: Imply0[p, d]): Imply0[p, d] = i
}
