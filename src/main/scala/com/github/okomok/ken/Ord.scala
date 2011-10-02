

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


trait Ord[-a] extends Eq[a] {
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

    def max[b <: a](x: b)(y: b): b = if (op_<=(x)(y)) y else x
    def min[b <: a](x: b)(y: b): b = if (op_<=(x)(y)) x else y

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

    private[ken] sealed class Op_max_[b <: a](x: b) {
        def _max_(y: b): b = max(x)(y)
    }
    final implicit def _max_[b <: a](x: b): Op_max_[b] = new Op_max_(x)

    private[ken] sealed class Op_min_[b <: a](x: b) {
        def _min_(y: b): b = min(x)(y)
    }
    final implicit def _min_[b <: a](x: b): Op_min_[b] = new Op_min_(x)
}


trait OrdProxy[-a] extends Ord[a] with EqProxy[a] {
    def selfOrd: Ord[a]
    override def selfEq: Eq[a] = selfOrd

    override def compare: compare = selfOrd.compare
    override def op_< : op_< = selfOrd.op_<
    override def op_<= : op_<= = selfOrd.op_<=
    override def op_> : op_> = selfOrd.op_>
    override def op_>= : op_>= = selfOrd.op_>=
    override def max[b <: a](x: b)(y: b): b = selfOrd.max(x)(y)
    override def min[b <: a](x: b)(y: b): b = selfOrd.min(x)(y)
}


object Ord extends OrdInstance with OrdShortcut {
    def apply[a <: Kind.Function0](implicit i: Ord[a#apply0]): Ord[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Ord[nt#oldtype0]): Ord[nt#apply0] = new Ord[nt#apply0] with EqProxy[nt#apply0] {
        override val selfEq = Eq.deriving[nt]
        private type a = nt#apply0

        override val compare: compare = x => y => i.compare(j.oldOf(x))(j.oldOf(y))
        override val op_< : op_< = x => y => i.op_<(j.oldOf(x))(j.oldOf(y))
        override val op_<= : op_<= = x => y => i.op_<=(j.oldOf(x))(j.oldOf(y))
        override val op_> : op_> = x => y => i.op_>(j.oldOf(x))(j.oldOf(y))
        override val op_>= : op_>= = x => y => i.op_>=(j.oldOf(x))(j.oldOf(y))
    }

    def weak[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Ord[nt#apply0]): Ord[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](j.coNewtype, i)

    def asScalaOrdering[a](implicit i: Ord[a]): scala.Ordering[a] = new scala.Ordering[a] {
        override def compare(x: a, y: a): Int = i.compare(x)(y) match {
            case EQ => 0
            case LT => -1
            case GT => 1
        }
        override def lteq(x: a, y: a): Boolean = i.op_<=(x)(y)
        override def gteq(x: a, y: a): Boolean = i.op_>=(x)(y)
        override def lt(x: a, y: a): Boolean = i.op_<(x)(y)
        override def gt(x: a, y: a): Boolean = i.op_>(x)(y)
    }
}


private[ken] sealed trait OrdInstance0 { this: Ord.type =>

    // Primitives
    //
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
    }

    // Tuples
    //
    implicit def ofTuple2[T1, T2](implicit ord1: Ord[T1], ord2: Ord[T2]): Ord[(T1, T2)] = new Ord[(T1, T2)]{
        override val compare: compare = x => y => {
            val compare1 = ord1.compare(x._1)(y._1)
            if (compare1 != EQ) compare1
            else {
                val compare2 = ord2.compare(x._2)(y._2)
                if (compare2 != EQ) compare2
                else {
                    EQ
                }
            }
        }
    }

    implicit def Tuple3[T1, T2, T3](implicit ord1: Ord[T1], ord2: Ord[T2], ord3: Ord[T3]) : Ord[(T1, T2, T3)] = new Ord[(T1, T2, T3)]{
        override val compare: compare = x => y => {
            val compare1 = ord1.compare(x._1)(y._1)
            if (compare1 != EQ) compare1
            else {
                val compare2 = ord2.compare(x._2)(y._2)
                if (compare2 != EQ) compare2
                else {
                    val compare3 = ord3.compare(x._3)(y._3)
                    if (compare3 != EQ) compare3
                    else {
                        EQ
                    }
                }
            }
        }
    }

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit j: Newtype0[nt, ot, ds], i: Ord[ot], k: Kind.MethodList.Contains[ds, Real]): Ord[nt] = deriving[Newtype0[nt, ot, _]]
}

sealed trait OrdInstance extends OrdInstance0 { this: Ord.type =>
    implicit val ofNothing: Ord[Nothing] with HighPriority = new Ord[Nothing] with HighPriority {}
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
