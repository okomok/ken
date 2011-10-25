

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


object Int extends Bounded[Int] with Enum[Int] with Eq.Of[Int]
    with Integral[Int] with Ix[Int] with Random[Int] with Show[Int]
{
    // Overrides
    //
    // Bounded
    override val minBound: minBound = JInt.MIN_VALUE
    override val maxBound: maxBound = JInt.MAX_VALUE
    // Enum
    override val succ: succ = x => {
        if (x == maxBound) error("Enum[Int].succ: tried to take `succ` of maxBound")
        else x + 1
    }
    override val pred: pred = x => {
        if (x == minBound) error("Enum[Int].pred: tried to take `pred` of minBound")
        else x - 1
    }
    override val toEnum: toEnum = x => x
    override val fromEnum: fromEnum = x => x
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
    // Num
    override val op_+ : op_+ = x => y => x + y
    override val op_- : op_- = x => y => x - y
    override val op_* : op_* = x => y => x * y
    override val negate: negate = n => -n
    override val abs: abs = n => if (n > 0) n else -n
    override val signum: signum = n => {
        if (n < 0) -1
        else if (n == 0) 0
        else 1
    }
    override val fromInteger: fromInteger = i => i.toInt
    // Real
    override val toRational: toRational = x => Ratio(toInteger(x), 1)
    // Integral
    override val quot: quot = a => b => a / b
    override val rem: rem = a => b => a % b
    override val quotRem: quotRem = a => b => (a / b, a % b)
    override val toInteger: toInteger = i => i
    // Ix
    override val range: range = { case (m, n) => enumFromTo(m)(n) }
    override val unsafeIndex: unsafeIndex = { case (m, _n) => i => i - m }
    override val index: index = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i)
        else indexError(b)(i)("Int")
    }
    override val inRange: inRange = { case (m, n) => i => m <= i && i <= n }
    // Random
    private type a = Int
    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = ival match {
        case (a, b) => Random.randomIvalInteger[g, a](toInteger(a), toInteger(b))(g)
    }
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = randomR(minBound, maxBound)(g)
    // Show
    override val showsPrec: showsPrec = showSignedInt

    private def showSignedInt: Int => Int => ShowS = p => n => r => {
        if (n < 0 && p > 6) '(' :: itos(n)(')' :: r)
        else itos(n)(r)
    }

    private val itos: Int => String => String = n => cs => {
        n.toString ++: cs
    }
}
