

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


// `object Bool` crashes scalac.
private[ken] object _Bool extends Bounded[Bool] with Enum[Bool] with Eq.Of[Bool]
    with Ord[Bool] with Ix[Bool] with Show.Of[Bool]
{
    // Overrides
    //
    // Bounded
    override val minBound: minBound = False
    override val maxBound: maxBound = True
    // Enum
    override val succ: succ = b => {
        if (b == False) True
        else error("Enum[Bool].succ: bad argument")
    }
    override val pred: pred = b => {
        if (b == True) False
        else error("Enum[Bool].pred: bad argument")
    }
    override val toEnum: toEnum = n => {
        if (n == 0) False
        else if (n == 1) True
        else error("Enum[Bool].toEnum: bad argument")
    }
    override val fromEnum: fromEnum = b => {
        if (b == False) 0
        else 1
    }
    // Ord
    override val op_< : op_< = b1 => b2 => fromEnum(b1) < fromEnum(b2)
    override val op_<= : op_<= = b1 => b2 => fromEnum(b1) <= fromEnum(b2)
    override val op_> : op_> = b1 => b2 => fromEnum(b1) > fromEnum(b2)
    override val op_>= : op_>= = b1 => b2 => fromEnum(b1) >= fromEnum(b2)
    // Ix
    override val range: range = { case (m, n) => enumFromTo(m)(n) }
    override val unsafeIndex: unsafeIndex = { case (l, _) => i => fromEnum(i) - fromEnum(l) }
    override val index: index = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i)
        else indexError(b)(i)("Bool")
    }
    override val inRange: inRange = { case (l, u) => i => fromEnum(i) >= fromEnum(l) && fromEnum(i) <= fromEnum(u) }

    // Prelude
    //
    val not: Bool => Bool = b => !b

    val op_&& : Bool => Lazy[Bool] => Bool = b => c => b && c.!

    val op_|| : Bool => Lazy[Bool] => Bool = b => c => b || c.!
}
