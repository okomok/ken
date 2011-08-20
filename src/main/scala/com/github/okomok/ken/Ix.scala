

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Ix[a] extends Ord[a] {
    final val asIx: Ix[apply] = this

    // Core
    //
    def range: Tuple2[a, a] => List[a]
    def index: Tuple2[a, a] => a => Int = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i) else error("Error in array index")
    }

    def unsafeIndex: Tuple2[a, a] => a => Int
    def inRange: Tuple2[a, a] => a => Bool
    def rangeSize: Tuple2[a, a] => Int = {
        case b@(_l, h) => if (inRange(b)(h)) (unsafeIndex(b)(h) + 1) else 0
    }
    def unsafeRangeSize: Tuple2[a, a] => Int = {
        case b@(_l, h) => unsafeIndex(b)(h) + 1
    }

    // Extra
    //
    final val indexError: Tuple2[a, a] => a => String_ => Nothing = rng => i => tp => {
        import Show._
        error( (showString("Ix{") compose showString(tp) compose showString("}.index: Index ") compose
            showParen(True)(Show[a].showsPrec(0)(i)) compose
            showString(" out of range ")) {
                showParen(True)(Show[(a, a)].showsPrec(0)(rng))("")
            } )
    }
}


trait IxProxy[a] extends Ix[a] with OrdProxy[a] {
    def selfIx: Ix[a]
    override def selfOrd: Ord[a] = selfIx

    override def range: Tuple2[a, a] => List[a] = selfIx.range
    override def index: Tuple2[a, a] => a => Int = selfIx.index
    override def unsafeIndex: Tuple2[a, a] => a => Int = selfIx.unsafeIndex
    override def inRange: Tuple2[a, a] => a => Bool = selfIx.inRange
    override def rangeSize: Tuple2[a, a] => Int = selfIx.rangeSize
    override def unsafeRangeSize: Tuple2[a, a] => Int = selfIx.unsafeRangeSize
}


object Ix {
    def apply[a <: Kind.Function0](implicit i: Ix[a#apply0]): Ix[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Ix[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Ix[nt#apply0] = new Ix[nt#apply0] with OrdProxy[nt#apply0] {
        private[this] type a = nt#apply0
        override val selfOrd = Ord.deriving[nt, ot](i, j)
        override val range: Tuple2[a, a] => List[a] = t => List.map[ot#apply0, a](j.newOf)(i.range(j.oldOf(t._1), j.oldOf(t._2)))
        override val index: Tuple2[a, a] => a => Int = t => x => i.index(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val unsafeIndex: Tuple2[a, a] => a => Int = t => x => i.unsafeIndex(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val inRange: Tuple2[a, a] => a => Bool = t => x => i.inRange(j.oldOf(t._1), j.oldOf(t._2))(j.oldOf(x))
        override val rangeSize: Tuple2[a, a] => Int = t => i.rangeSize(j.oldOf(t._1), j.oldOf(t._2))
        override val unsafeRangeSize: Tuple2[a, a] => Int = t => i.unsafeRangeSize(j.oldOf(t._1), j.oldOf(t._2))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Ix[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Ix[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
}
