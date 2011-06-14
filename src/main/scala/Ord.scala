

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Ord[a] {
    def compare(x: a)(y: a): Ordering = if (x == y) EQ else if (op_<=(x)(y)) LT else GT
    def op_<(x: a)(y: a): Boolean = compare(x)(y) match { case LT => true; case _ => false }
    def op_<=(x: a)(y: a): Boolean = compare(x)(y) match { case GT => false; case _ => true }
    def op_>(x: a)(y: a): Boolean = compare(x)(y) match { case GT => true; case _ => false }
    def op_>=(x: a)(y: a): Boolean = compare(x)(y) match { case LT => false; case _ => true }
    def max(x: a)(y: a): a = if (op_<=(x)(y)) y else x
    def min(x: a)(y: a): a = if (op_<=(x)(y)) x else y
}


object Ord {
    def op_<[a](x: a)(y: a)(implicit i: Ord[a]): Boolean = i.op_<(x)(y)
    def op_<=[a](x: a)(y: a)(implicit i: Ord[a]): Boolean = i.op_<=(x)(y)
    def op_>[a](x: a)(y: a)(implicit i: Ord[a]): Boolean = i.op_>(x)(y)
    def op_>=[a](x: a)(y: a)(implicit i: Ord[a]): Boolean = i.op_>=(x)(y)
    def max[a](x: a)(y: a)(implicit i: Ord[a]): a = i.max(x)(y)
    def min[a](x: a)(y: a)(implicit i: Ord[a]): a = i.min(x)(y)

    private[ken] class Op_<[a](x: a)(implicit i: Ord[a]) {
        def <(y: a): Boolean = op_<(x)(y)
    }
    implicit def <[a](x: a)(implicit i: Ord[a]): Op_<[a] = new Op_<[a](x)

    private[ken] class Op_<=[a](x: a)(implicit i: Ord[a]) {
        def <=(y: a): Boolean = op_<=(x)(y)
    }
    implicit def <=[a](x: a)(implicit i: Ord[a]): Op_<=[a] = new Op_<=[a](x)

    private[ken] class Op_>[a](x: a)(implicit i: Ord[a]) {
        def >(y: a): Boolean = op_>(x)(y)
    }
    implicit def >[a](x: a)(implicit i: Ord[a]): Op_>[a] = new Op_>[a](x)

    private[ken] class Op_>=[a](x: a)(implicit i: Ord[a]) {
        def >=(y: a): Boolean = op_>=(x)(y)
    }
    implicit def >=[a](x: a)(implicit i: Ord[a]): Op_>=[a] = new Op_>=[a](x)

    implicit def instanceOfOrdering[a](implicit i: scala.Ordering[a]): Ord[a] = new Ord[a] {
        override def compare(x: a)(y: a): Ordering = i.compare(x, y) match {
            case 0 => EQ
            case s if s < 0 => LT
            case s if s > 0 => GT
        }
        override def op_<(x: a)(y: a): Boolean = i.lt(x, y)
        override def op_<=(x: a)(y: a): Boolean = i.lteq(x, y)
        override def op_>(x: a)(y: a): Boolean = i.gt(x, y)
        override def op_>=(x: a)(y: a): Boolean = i.gteq(x, y)
        override def max(x: a)(y: a): a = i.max(x, y)
        override def min(x: a)(y: a): a = i.min(x, y)
    }
}
