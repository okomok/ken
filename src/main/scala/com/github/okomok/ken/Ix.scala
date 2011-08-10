

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
    override def self: Ix[a]

    override def range: Tuple2[a, a] => List[a] = self.range
    override def index: Tuple2[a, a] => a => Int = self.index
    override def unsafeIndex: Tuple2[a, a] => a => Int = self.unsafeIndex
    override def inRange: Tuple2[a, a] => a => Bool = self.inRange
    override def rangeSize: Tuple2[a, a] => Int = self.rangeSize
    override def unsafeRangeSize: Tuple2[a, a] => Int = self.unsafeRangeSize
}


object Ix {
    def apply[a](implicit i: Ix[a]): Ix[a] = i
}
