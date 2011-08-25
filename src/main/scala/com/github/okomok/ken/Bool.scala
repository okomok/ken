

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


object Bool extends Bounded[Bool] with Enum[Bool] with Ord[Bool] with Show[Bool] {
    // Overrides
    //
    // Bounded
    private[this] type a = Boolean // Bool alias make it crash.
    override val minBound: a = False
    override val maxBound: a = True
    // Enum
    override val succ: a => a = b => {
        if (b == False) True
        else error("Enum[Bool].succ: bad argument")
    }
    override val pred: a => a = b => {
        if (b == True) False
        else error("Enum[Bool].pred: bad argument")
    }
    override val toEnum: Int => a = n => {
        if (n == 0) False
        else if (n == 1) True
        else error("Enum[Bool].toEnum: bad argument")
    }
    override val fromEnum: a => Int = b => {
        if (b == False) 0
        else 1
    }
    // Eq
    override val op_=== : a => a => Bool = b1 => b2 => b1 == b2
    override val op_/== : a => a => Bool = b1 => b2 => b1 != b2
    // Ord
    override val op_< : a => a => Bool = b1 => b2 => fromEnum(b1) < fromEnum(b2)
    override val op_<= : a => a => Bool = b1 => b2 => fromEnum(b1) <= fromEnum(b2)
    override val op_> : a => a => Bool = b1 => b2 => fromEnum(b1) > fromEnum(b2)
    override val op_>= : a => a => Bool = b1 => b2 => fromEnum(b1) >= fromEnum(b2)
    // Show
    override val showsPrec: Int => a => ShowS = _ => a => Show.showString(a.toString)
}
