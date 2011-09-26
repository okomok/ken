

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


trait Ix[a] extends Ord[a] {
    final val asIx: Ix[apply0] = this

    // Core
    //
    type range = Pair[a, a] => List[a]
    def range: range

    type index = Pair[a, a] => a => Int
    def index: index = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i) else error("Error in array index")
    }

    type unsafeIndex = Pair[a, a] => a => Int
    def unsafeIndex: unsafeIndex

    type inRange = Pair[a, a] => a => Bool
    def inRange: inRange

    type rangeSize = Pair[a, a] => Int
    def rangeSize: rangeSize = {
        case b @ (_l, h) => if (inRange(b)(h)) (unsafeIndex(b)(h) + 1) else 0
    }

    type unsafeRangeSize = Pair[a, a] => Int
    def unsafeRangeSize: unsafeRangeSize = {
        case b @ (_l, h) => unsafeIndex(b)(h) + 1
    }

    // Extra
    //
    final def indexError(rng: (a, a))(i: a)(tp: String)(implicit j: Show[a]): Nothing = {
        import Show._
        error( (showString("Ix{") `.` showString(tp) `.` showString("}.index: Index ") `.`
            showParen(True)(showsPrec(0)(i)) `.`
            showString(" out of range ")) {
                showParen(True)(showsPrec(0)(rng))("")
            } )
    }

    type safeRangeSize = Pair[a, a] => Int
    final val safeRangeSize: safeRangeSize = { case (l, u) =>
        val r = rangeSize(l, u)
        if (r < 0) error("Negative range size")
        else r
    }

    type safeIndex = Pair[a, a] => Int => a => Int
    final val safeIndex: safeIndex = { case (l, u) => n => i =>
        val i_ = index(l, u)(i)
        if (0 <= i_ && i_ < n) i_
        else error("Error in array index; " ++: Show.show(i_) ++: " not in range [0.." ++: Show.show(n) ++: List.from(")"))
    }
}


trait IxProxy[a] extends Ix[a] with OrdProxy[a] {
    def selfIx: Ix[a]
    override def selfOrd: Ord[a] = selfIx

    override def range: range = selfIx.range
    override def index: index = selfIx.index
    override def unsafeIndex: unsafeIndex = selfIx.unsafeIndex
    override def inRange: inRange = selfIx.inRange
    override def rangeSize: rangeSize = selfIx.rangeSize
    override def unsafeRangeSize: unsafeRangeSize = selfIx.unsafeRangeSize
}


object Ix extends IxInstance with IxShortcut {
    def apply[a <: Kind.Function0](implicit i: Ix[a#apply0]): Ix[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: Ix[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Ix[nt#apply0] = new Ix[nt#apply0] with OrdProxy[nt#apply0] {
        private type a = nt#apply0
        override val selfOrd = Ord.deriving[nt](i, j)

        override val range: range = t => List.map[nt#oldtype0, a](j.newOf)(i.range(j.oldOf(t._1), j.oldOf(t._2)))
        override val index: index = t => x => i.index(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val unsafeIndex: unsafeIndex = t => x => i.unsafeIndex(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val inRange: inRange = t => x => i.inRange(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val rangeSize: rangeSize = t => i.rangeSize(j.oldOf(t._1), j.oldOf(t._2))
        override val unsafeRangeSize: unsafeRangeSize = t => i.unsafeRangeSize(j.oldOf(t._1), j.oldOf(t._2))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Ix[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Ix[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](i, j.coNewtype)
}


sealed trait IxInstance { this: Ix.type =>
    implicit val ofBool: Ix[Bool] = _Bool
    implicit val ofInt: Ix[Int] = Int
    implicit val ofInteger: Ix[Integer] = _Integer
    implicit val ofOrdering: Ix[Ordering] = Ordering
    implicit val ofUnit: Ix[Unit] = Unit

    implicit def ofScalaNumeric[a](implicit i: scala.Numeric[a]): Ix[a] = new Ix[a] with OrdProxy[a] {
        override val selfOrd = Ord.ofScalaOrdering(i)
        override val range: range = { case (n, m) =>
            Predef.require(i.lteq(n, m))
            if (i.equiv(n, m)) Nil else n :: range(i.plus(n, i.one), m)
        }
        override val unsafeIndex: unsafeIndex = { case (n, _) => k => i.toInt(i.minus(k, n)) }
        override val index: index = b => i => {
            if (inRange(b)(i)) unsafeIndex(b)(i) else indexError(b)(i)("Integer")
        }
        override val inRange: inRange = { case (n, m) => k => i.lteq(n, k) && i.lteq(k, m) }
    }

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Ix[ot], k: Kind.MethodList.Contains[ds, Ix]): Ix[nt] = deriving[Newtype0[nt, ot, _]]
}


sealed trait IxShortcut { this: Ix.type =>
    def range[a](b: (a, a))(implicit ix: Ix[a]): List[a] = ix.range(b)
    def index[a](b: (a, a))(i: a)(implicit ix: Ix[a]): Int = ix.index(b)(i)
    def unsafeIndex[a](b: (a, a))(i: a)(implicit ix: Ix[a]): Int = ix.unsafeIndex(b)(i)
    def inRange[a](b: (a, a))(i: a)(implicit ix: Ix[a]): Bool = ix.inRange(b)(i)
    def rangeSize[a](b: (a, a))(implicit ix: Ix[a]): Int = ix.rangeSize(b)
    def unsafeRangeSize[a](b: (a, a))(implicit ix: Ix[a]): Int = ix.unsafeRangeSize(b)
    def indexError[a](rng: (a, a))(i: a)(tp: String)(implicit ix : Ix[a], j: Show[a]): Nothing = ix.indexError(rng)(i)(tp)
}
