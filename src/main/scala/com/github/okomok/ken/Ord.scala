

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Ord[a] extends Eq[a] { outer =>
    final val asOrd: Ord[apply0] = this

    // Core
    //
    type compare = a => a => Ordering
    def compare: compare = x => y => if (x === y) EQ else if (op_<=(x)(y)) LT else GT

    type op_< = a => a => Bool
    def op_< : op_< = x => y => compare(x)(y) match { case LT => True; case _ => False }

    type op_<= = a => a => Bool
    def op_<= : op_<= = x => y => compare(x)(y) match { case GT => False; case _ => True }

    type op_> = a => a => Bool
    def op_> : op_> = x => y => compare(x)(y) match { case GT => True; case _ => False }

    type op_>= = a => a => Bool
    def op_>= : op_>= = x => y => compare(x)(y) match { case LT => False; case _ => True }

    type max = a => a => a
    def max: max = x => y => if (op_<=(x)(y)) y else x

    type min = a => a => a
    def min: min = x => y => if (op_<=(x)(y)) x else y

    // Operators
    //
    private[ken] sealed class Op_<(x: a) {
        def <(y: a): Bool = op_<(x)(y)
    }
    final implicit def <(x: a): Op_< = new Op_<(x)

    private[ken] sealed class Op_<=(x: a) {
        def <=(y: a): Bool = op_<=(x)(y)
    }
    final implicit def <=(x: a): Op_<= = new Op_<=(x)

    private[ken] sealed class Op_>(x: a) {
        def >(y: a): Bool = op_>(x)(y)
    }
    final implicit def >(x: a): Op_> = new Op_>(x)

    private[ken] sealed class Op_>=(x: a) {
        def >=(y: a): Bool = op_>=(x)(y)
    }
    final implicit def >=(x: a): Op_>= = new Op_>=(x)

    private[ken] sealed class Op_max_(x: a) {
        def _max_(y: a): a = max(x)(y)
    }
    final implicit def _max_(x: a): Op_max_ = new Op_max_(x)

    private[ken] sealed class Op_min_(x: a) {
        def _min_(y: a): a = min(x)(y)
    }
    final implicit def _min_(x: a): Op_min_ = new Op_min_(x)
}


trait OrdProxy[a] extends Ord[a] with EqProxy[a] {
    def selfOrd: Ord[a]
    override def selfEq: Eq[a] = selfOrd

    override def compare: compare = selfOrd.compare
    override def op_< : op_< = selfOrd.op_<
    override def op_<= : op_<= = selfOrd.op_<=
    override def op_> : op_> = selfOrd.op_>
    override def op_>= : op_>= = selfOrd.op_>=
    override def max: max = selfOrd.max
    override def min: min = selfOrd.min
}


object Ord extends OrdInstance with OrdShortcut {
    def apply[a <: Kind.Function0](implicit i: Ord[a#apply0]): Ord[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Ord[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Ord[nt#apply0] = new Ord[nt#apply0] with EqProxy[nt#apply0] {
        override val selfEq = Eq.deriving[nt, ot]

        override val compare: compare = x => y => i.compare(j.oldOf(x))(j.oldOf(y))
        override val op_< : op_< = x => y => i.op_<(j.oldOf(x))(j.oldOf(y))
        override val op_<= : op_<= = x => y => i.op_<=(j.oldOf(x))(j.oldOf(y))
        override val op_> : op_> = x => y => i.op_>(j.oldOf(x))(j.oldOf(y))
        override val op_>= : op_>= = x => y => i.op_>=(j.oldOf(x))(j.oldOf(y))
        override val max: max = x => y => j.newOf(i.max(j.oldOf(x))(j.oldOf(y)))
        override val min: min = x => y => j.newOf(i.min(j.oldOf(x))(j.oldOf(y)))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Ord[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Ord[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
}


sealed trait OrdInstance { this: Ord.type =>
    implicit val ofBool: Ord[Bool] = _Bool
    implicit val ofChar: Ord[Char] = Char
    implicit val ofDouble: Ord[Double] = Double
    implicit val ofFloat: Ord[Float] = Float
    implicit val ofInt: Ord[Int] = Int
    implicit val ofInteger: Ord[Integer] = _Integer

    implicit def ofScalaOrdering[a](implicit i: scala.Ordering[a]): Ord[a] = new Ord[a] with EqProxy[a] {
        override val selfEq = Eq.ofScalaEquiv(i)
        override def compare: compare = x => y => i.compare(x, y) match {
            case 0 => EQ
            case s if s < 0 => LT
            case s if s > 0 => GT
        }
        override val op_< : op_< = x => y => i.lt(x, y)
        override val op_<= : op_<= = x => y => i.lteq(x, y)
        override val op_> : op_> = x => y => i.gt(x, y)
        override val op_>= : op_>= = x => y => i.gteq(x, y)
        override val max: max = x => y => i.max(x, y)
        override val min: min = x => y => i.min(x, y)
    }
}


sealed trait OrdShortcut { this: Ord.type =>
    def compare[a](x: a)(y: a)(implicit i: Ord[a]): Ordering = i.compare(x)(y)
    def op_<[a](x: a)(y: a)(implicit i: Ord[a]): Bool = i.op_<(x)(y)
    def op_<=[a](x: a)(y: a)(implicit i: Ord[a]): Bool = i.op_<=(x)(y)
    def op_>[a](x: a)(y: a)(implicit i: Ord[a]): Bool = i.op_>(x)(y)
    def op_>=[a](x: a)(y: a)(implicit i: Ord[a]): Bool = i.op_>=(x)(y)
    def max[a](x: a)(y: a)(implicit i: Ord[a]): a = i.max(x)(y)
    def min[a](x: a)(y: a)(implicit i: Ord[a]): a = i.min(x)(y)

    private[ken] class _Op_<[a](x: a)(implicit i: Ord[a]) {
        def <(y: a): Bool = op_<(x)(y)
    }
    implicit def <[a](x: a)(implicit i: Ord[a]): _Op_<[a] = new _Op_<(x)

    private[ken] class _Op_<=[a](x: a)(implicit i: Ord[a]) {
        def <=(y: a): Bool = op_<=(x)(y)
    }
    implicit def <=[a](x: a)(implicit i: Ord[a]): _Op_<=[a] = new _Op_<=(x)

    private[ken] class _Op_>[a](x: a)(implicit i: Ord[a]) {
        def >(y: a): Bool = op_>(x)(y)
    }
    implicit def >[a](x: a)(implicit i: Ord[a]): _Op_>[a] = new _Op_>(x)

    private[ken] class _Op_>=[a](x: a)(implicit i: Ord[a]) {
        def >=(y: a): Bool = op_>=(x)(y)
    }
    implicit def >=[a](x: a)(implicit i: Ord[a]): _Op_>=[a] = new _Op_>=(x)

    private[ken] class _Op_max_[a](x: a)(implicit i: Ord[a]) {
        def _max_(y: a): a = max(x)(y)
    }
    implicit def _max_[a](x: a)(implicit i: Ord[a]): _Op_max_[a] = new _Op_max_(x)

    private[ken] class _Op_min_[a](x: a)(implicit i: Ord[a]) {
        def _min_(y: a): a = min(x)(y)
    }
    implicit def _min_[a](x: a)(implicit i: Ord[a]): _Op_min_[a] = new _Op_min_(x)
}
