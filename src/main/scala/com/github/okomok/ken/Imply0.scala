

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Implements `d` instances from `p` instances.
 */
trait Imply0[p, d] extends Typeclass { outer =>
    final val asImply0: Imply0[p, d] = this

    // Core
    //
    def imply0(p: p): d
    def unimply0(d: => d): p

    // Instances
    //
    def Eq(implicit i: Eq[p]): Eq[d] = new Eq[d] {
        private[this] type a = d
        override val op_== : a => a => Bool = x => y => i.op_==(unimply0(x))(unimply0(y))
        override val op_/= : a => a => Bool = x => y => i.op_/=(unimply0(x))(unimply0(y))
    }

    def Ord(implicit i: Ord[p]): Ord[d] = new Ord[d] with EqProxy[d] {
        private[this] type a = d
        override val self = outer.Eq(i)
        override val compare: a => a => Ordering = x => y => i.compare(unimply0(x))(unimply0(y))
        override val op_< : a => a => Bool = x => y => i.op_<(unimply0(x))(unimply0(y))
        override val op_<= : a => a => Bool = x => y => i.op_<=(unimply0(x))(unimply0(y))
        override val op_> : a => a => Bool = x => y => i.op_>(unimply0(x))(unimply0(y))
        override val op_>= : a => a => Bool = x => y => i.op_>=(unimply0(x))(unimply0(y))
        override val max: a => a => a = x => y => imply0(i.max(unimply0(x))(unimply0(y)))
        override val min: a => a => a = x => y => imply0(i.min(unimply0(x))(unimply0(y)))
    }

    def Ix(implicit i: Ix[p]): Ix[d] = new Ix[d] with OrdProxy[d] {
        private[this] type a = d
        override val self = outer.Ord(i)
        override val range: Tuple2[a, a] => List[a] = t => List.map[p, a](imply0)(i.range(unimply0(t._1), unimply0(t._2)))
        override val index: Tuple2[a, a] => a => Int = t => x => i.index(unimply0(t._1), unimply0(t._2))(unimply0(x))
        override val unsafeIndex: Tuple2[a, a] => a => Int = t => x => i.unsafeIndex(unimply0(t._1), unimply0(t._2))(unimply0(x))
        override val inRange: Tuple2[a, a] => a => Bool = t => x => i.inRange(unimply0(t._1), unimply0(t._2))(unimply0(x))
        override val rangeSize: Tuple2[a, a] => Int = t => i.rangeSize(unimply0(t._1), unimply0(t._2))
        override val unsafeRangeSize: Tuple2[a, a] => Int = t => i.unsafeRangeSize(unimply0(t._1), unimply0(t._2))
    }

    def Monoid(implicit i: Monoid[p]): Monoid[d] = new Monoid[d] {
        private[this] type m = d
        override val mempty: m = imply0(i.mempty)
        override val mappend: m => (=> m) => m = x => y => imply0(i.mappend(unimply0(x))(unimply0(y)))
        override val mconcat: List[m] => m = xs => imply0(i.mconcat(List.map[m, p](Function.!(unimply0))(xs)))
    }
}


trait Imply0Proxy[p, d] extends Imply0[p, d] with Proxy {
    override def self: Imply0[p, d]

    override def imply0(p: p): d = self.imply0(p)
    override def unimply0(d: => d): p = self.unimply0(d)

    override def Eq(implicit i: Eq[p]): Eq[d] = self.Eq(i)
    override def Ord(implicit i: Ord[p]): Ord[d] = self.Ord(i)
    override def Ix(implicit i: Ix[p]): Ix[d] = self.Ix(i)
    override def Monoid(implicit i: Monoid[p]): Monoid[d] = self.Monoid(i)
}


object Imply0 {
    def apply[p, d](implicit i: Imply0[p, d]): Imply0[p, d] = i
}
