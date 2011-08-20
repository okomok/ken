

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Ord[a] extends Eq[a] { outer =>
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

    // Operators
    //
    sealed class Op_<(x: a) {
        def <(y: a): Bool = op_<(x)(y)
    }
    final implicit def <(x: a): Op_< = new Op_<(x)

    sealed class Op_<=(x: a) {
        def <=(y: a): Bool = op_<=(x)(y)
    }
    final implicit def <=(x: a): Op_<= = new Op_<=(x)

    sealed class Op_>(x: a) {
        def >(y: a): Bool = op_>(x)(y)
    }
    final implicit def >(x: a): Op_> = new Op_>(x)

    sealed class Op_>=(x: a) {
        def >=(y: a): Bool = op_>=(x)(y)
    }
    final implicit def >=(x: a): Op_>= = new Op_>=(x)
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
    def apply[a <: Kind.Function0](implicit i: Ord[a#apply0]): Ord[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Ord[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Ord[nt#apply0] = new Ord[nt#apply0] with EqProxy[nt#apply0] {
        private[this] type a = nt#apply0
        override val self = Eq.deriving[nt, ot](i, j)
        override val compare: a => a => Ordering = x => y => i.compare(j.oldOf(x))(j.oldOf(y))
        override val op_< : a => a => Bool = x => y => i.op_<(j.oldOf(x))(j.oldOf(y))
        override val op_<= : a => a => Bool = x => y => i.op_<=(j.oldOf(x))(j.oldOf(y))
        override val op_> : a => a => Bool = x => y => i.op_>(j.oldOf(x))(j.oldOf(y))
        override val op_>= : a => a => Bool = x => y => i.op_>=(j.oldOf(x))(j.oldOf(y))
        override val max: a => a => a = x => y => j.newOf(i.max(j.oldOf(x))(j.oldOf(y)))
        override val min: a => a => a = x => y => j.newOf(i.min(j.oldOf(x))(j.oldOf(y)))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Ord[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Ord[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
}
