

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Ord[a] extends Eq[a] {
    final val asOrd: Ord[apply] = this

    // Core
    //
    def compare: a => a => Ordering = { x => y => if (x == y) EQ else if (op_<=(x)(y)) LT else GT }
    def op_< : a => a => Bool = { x => y => compare(x)(y) match { case LT => True; case _ => False } }
    def op_<= : a => a => Bool = { x => y => compare(x)(y) match { case GT => False; case _ => True } }
    def op_> : a => a => Bool = { x => y => compare(x)(y) match { case GT => True; case _ => False } }
    def op_>= : a => a => Bool = { x => y => compare(x)(y) match { case LT => False; case _ => True } }
    def max: a => a => a = { x => y => if (op_<=(x)(y)) y else x }
    def min: a => a => a = { x => y => if (op_<=(x)(y)) x else y }

    // Infix
    //
    sealed class Infix_<(x: a) {
        def <(y: a): Bool = op_<(x)(y)
    }
    final implicit def <(x: a): Infix_< = new Infix_<(x)

    sealed class Infix_<=(x: a) {
        def <=(y: a): Bool = op_<=(x)(y)
    }
    final implicit def <=(x: a): Infix_<= = new Infix_<=(x)

    sealed class Infix_>(x: a) {
        def >(y: a): Bool = op_>(x)(y)
    }
    final implicit def >(x: a): Infix_> = new Infix_>(x)

    sealed class Infix_>=(x: a) {
        def >=(y: a): Bool = op_>=(x)(y)
    }
    final implicit def >=(x: a): Infix_>= = new Infix_>=(x)
}


trait OrdProxy[a] extends Ord[a] with Proxy {
    override def self: Ord[a]

    override def compare: a => a => Ordering = self.compare
    override def op_< : a => a => Bool = self.op_<
    override def op_<= : a => a => Bool = self.op_<=
    override def op_> : a => a => Bool = self.op_>
    override def op_>= : a => a => Bool = self.op_>=
    override def max: a => a => a = self.max
    override def min: a => a => a = self.min
}


object Ord {
    def apply[a](implicit i: Ord[a]): Ord[a] = i

    implicit def _ofOrdering[a](implicit i: scala.Ordering[a]): Ord[a] = new Ord[a] with EqProxy[a] {
        override val self = Eq._ofEquiv[a]
        override val compare: a => a => Ordering = { x => y => i.compare(x, y) match {
            case 0 => EQ
            case s if s < 0 => LT
            case s if s > 0 => GT
        } }
        override val op_< : a => a => Bool = { x => y => i.lt(x, y) }
        override val op_<= : a => a => Bool = { x => y => i.lteq(x, y) }
        override val op_> : a => a => Bool = { x => y => i.gt(x, y) }
        override val op_>= : a => a => Bool = { x => y => i.gteq(x, y) }
        override val max: a => a => a = { x => y => i.max(x, y) }
        override val min: a => a => a = { x => y => i.min(x, y) }
    }
}
