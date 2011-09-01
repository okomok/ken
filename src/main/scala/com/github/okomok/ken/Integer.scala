

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


// `object Integer` crashes scalac.
private[ken] object _Integer extends Enum[Integer] with Eq.Of[Integer] with Integral[Integer] with Show[Integer] {
    // Overrides
    //
    // Enum
    override val succ: succ = x => x + 1
    override val pred: pred = x => x - 1
    override val toEnum: toEnum = n => n
    override val fromEnum: fromEnum = n => n.toInt
    override val enumFrom: enumFrom = e1 => {
        e1 :: enumFrom(e1 + 1)
    }
    override val enumFromThen: enumFromThen = e1 => e2 => {
        val i = e2 - e1
        e1 :: enumFromThen(e1 + i)(e2 + i)
    }
    override val enumFromTo: enumFromTo = e1 => e3 => { // inclusive
        if (e1 > e3) Nil
        else e1 :: enumFromTo(e1 + 1)(e3)
    }
    override val enumFromThenTo: enumFromThenTo = e1 => e2 => e3 => {
        val i = e2 - e1
        if (i >= 0) {
            if (e1 > e3) Nil
            else e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
        } else {
            if (e1 < e3) Nil
            else e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
        }
    }
    // Ord
    override val compare: compare = x => y => {
        if (x < y) LT
        else if (x == y) EQ
        else GT
    }
    override val op_< : op_< = x => y => x < y
    override val op_<= : op_<= = x => y => x <= y
    override val op_> : op_> = x => y => x > y
    override val op_>= : op_>= = x => y => x >= y
    override val max: max = x => y => x.max(y)
    override val min: min = x => y => x.min(y)
    // Num
    override val op_+ : op_+ = x => y => x + y
    override val op_- : op_- = x => y => x - y
    override val op_* : op_* = x => y => x * y
    override val negate: negate = n => -n
    override val abs: abs = n => n.abs
    override val signum: signum = n => n.signum
    override val fromInteger: fromInteger = x => x
    // Real
    override val toRational: toRational = x => Ratio(x, 1)
    // Integral
    override val quot: quot = a => b => a / b
    override val rem: rem = a => b => a % b
    override val quotRem: quotRem = a => b => (a / b, a % b)
    override val toInteger: toInteger = i => i
    // Show
    override val showsPrec: showsPrec = p => n => r => {
        if (p > 6 && n < 0) '(' :: integerToString(n)(')' :: r)
        else integerToString(n)(r)
    }

    private lazy val integerToString: Integer => String_ => String_ = n => cs => {
        n.toString ::: cs
    }
}
