

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


import java.lang.{Integer => JInt}


object Int extends Bounded[Int] with Enum[Int] with Eq.Of[Int] with Integral[Int] with Show[Int] {
    // Overrides
    //
    // Bounded
    private type a = Int
    override val minBound: a = JInt.MIN_VALUE
    override val maxBound: a = JInt.MAX_VALUE
    // Enum
    override val succ: a => a = x => {
        if (x == maxBound)  error("Enum[Int].succ: tried to take `succ` of maxBound")
        else x + 1
    }
    override val pred: a => a = x => {
        if (x == minBound) error("Enum[Int].pred: tried to take `pred` of minBound")
        else x - 1
    }
    override val toEnum: Int => a = x => x
    override val fromEnum: a => Int = x => x
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
    // Num
    override val op_+ : a => a => a = x => y => x + y
    override val op_- : a => a => a = x => y => x - y
    override val op_* : a => a => a = x => y => x * y
    override val negate: a => a = n => -n
    override val abs: a => a = n => if (n > 0) n else -n
    override val signum: a => a = n => {
        if (n < 0) -1
        else if (n == 0) 0
        else 1
    }
    override val fromInteger: Integer => a = i => i.toInt
    // Real
    override val toRational: a => Rational = x => Ratio(toInteger(x), 1)
    // Integral
    override val quot: a => a => a = a => b => a / b
    override val rem: a => a => a = a => b => a % b
    override val quotRem: a => a => (a, a) = a => b => (a / b, a % b)
    override val toInteger: a => Integer = i => i
    // Show
    override val showsPrec: Int => a => ShowS = showSignedInt

    private lazy val showSignedInt: Int => Int => ShowS = p => n => r => {
        if (n < 0 && p > 6) '(' :: itos(n)(')' :: r)
        else itos(n)(r)
    }

    private lazy val itos: Int => String_ => String_ = n => cs => {
        n.toString ::: cs
    }
}
