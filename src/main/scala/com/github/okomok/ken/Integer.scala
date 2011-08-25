

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


// Scalac is angry with the alias name.
object Integer extends Enum[BigInt] with Integral[BigInt] with Show[BigInt] {
    // Overrides
    //
    // Enum
    private type a = BigInt
    override val succ: a => a = x => x + 1
    override val pred: a => a = x => x - 1
    override val toEnum: Int => a = n => n
    override val fromEnum: a => Int = n => n.toInt
    override val enumFrom: a => List[a] = e1 => {
        e1 :: enumFrom(e1 + 1)
    }
    override val enumFromThen: a => a => List[a] = e1 => e2 => {
        val i = e2 - e1
        e1 :: enumFromThen(e1 + i)(e2 + i)
    }
    override val enumFromTo: a => a => List[a] = e1 => e3 => { // inclusive
        if (e1 > e3) Nil
        else e1 :: enumFromTo(e1 + 1)(e3)
    }
    override val enumFromThenTo: a => a => a => List[a] = e1 => e2 => e3 => {
        val i = e2 - e1
        if (i >= 0) {
            if (e1 > e3) Nil
            else e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
        } else {
            if (e1 < e3) Nil
            else e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
        }
    }
    // Eq
    override val op_=== : a => a => Bool = x => y => x == y
    override val op_/== : a => a => Bool = x => y => x != y
    // Ord
    override val compare: a => a => Ordering = x => y => {
        if (x < y) LT
        else if (x == y) EQ
        else GT
    }
    override val op_< : a => a => Bool = x => y => x < y
    override val op_<= : a => a => Bool = x => y => x <= y
    override val op_> : a => a => Bool = x => y => x > y
    override val op_>= : a => a => Bool = x => y => x >= y
    override val max: a => a => a = x => y => x.max(y)
    override val min: a => a => a = x => y => x.min(y)
    // Num
    override val op_+ : a => a => a = x => y => x + y
    override val op_- : a => a => a = x => y => x - y
    override val op_* : a => a => a = x => y => x * y
    override val negate: a => a = n => -n
    override val abs: a => a = n => n.abs
    override val signum: a => a = n => n.signum
    override val fromInteger: Integer => a = x => x
    // Real
    override val toRational: a => Rational = x => Ratio(x, 1)
    // Integral
    override val quot: a => a => a = a => b => a / b
    override val rem: a => a => a = a => b => a % b
    override val quotRem: a => a => (a, a) = a => b => (a / b, a % b)
    override val toInteger: a => Integer = i => i
    // Show
    override val showsPrec: Int => a => ShowS = p => n => r => {
        if (p > 6 && n < 0) '(' :: integerToString(n)(')' :: r)
        else integerToString(n)(r)
    }

    private lazy val integerToString: Integer => String_ => String_ = n => cs => {
        n.toString ::: cs
    }
}
