

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


trait RandomGen[g] extends Typeclass[g] {
    final val asRandomGen: RandomGen[apply0] = this

    // Core
    //
    type next = g => (Int, g)
    def next: next

    type split = g => (g, g)
    def split: split

    type genRange = Lazy[g] => (Int, Int)
    def genRange: genRange = _ => (Int.minBound, Int.maxBound)
}


trait RandomGenProxy[g] extends RandomGen[g] {
    def selfRandomGen: RandomGen[g]

    override def next: next = selfRandomGen.next
    override def split: split = selfRandomGen.split
    override def genRange: genRange = selfRandomGen.genRange
}


object RandomGen extends RandomGenInstance with RandomGenShortcut {
    def apply[g <: Kind.Function0](implicit i: RandomGen[g#apply0]): RandomGen[g#apply0] = i
}


sealed trait RandomGenInstance { this: RandomGen.type =>
}


sealed trait RandomGenShortcut { this: RandomGen.type =>
    def next[g](g: g)(implicit i: RandomGen[g]): (Int, g) = i.next(g)
    def split[g](g: g)(implicit i: RandomGen[g]): (g, g) = i.split(g)
    def genRange[g](g: Lazy[g])(implicit i: RandomGen[g]): (Int, Int) = i.genRange(g)
}
