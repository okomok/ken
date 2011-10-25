

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
private[ken] object _Integer extends Enum[Integer] with Eq.Of[Integer]
    with Integral[Integer] with Ix[Integer] with Random[Integer] with Show[Integer]
{
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
    private type a = Integer
    override val compare: compare = x => y => {
        if (x < y) LT
        else if (x == y) EQ
        else GT
    }
    override val op_< : op_< = x => y => x < y
    override val op_<= : op_<= = x => y => x <= y
    override val op_> : op_> = x => y => x > y
    override val op_>= : op_>= = x => y => x >= y
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
    // Ix
    override val range: range = { case (m, n) => enumFromTo(m)(n) }
    override val unsafeIndex: unsafeIndex = { case (m, _n) => i => Int.fromInteger(i - m) }
    override val index: index = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i)
        else indexError(b)(i)("Integer")
    }
    override val inRange: inRange = { case (m, n) => i => m <= i && i <= n }
    // Random
    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = Random.randomIvalInteger[g, a](ival)(g)
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = randomR(Int.toInteger(Int.minBound), Int.toInteger(Int.maxBound))(g)
    // Show
    override val showsPrec: showsPrec = p => n => r => {
        if (p > 6 && n < 0) '(' :: integerToString(n)(')' :: r)
        else integerToString(n)(r)
    }

    private lazy val integerToString: Integer => String => String = n => cs => {
        n.toString ++: cs
    }
}
