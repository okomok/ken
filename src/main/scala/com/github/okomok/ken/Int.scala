

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import java.lang.{Integer => JInt}


object Int extends IntegralProxy[Int] with Enum[Int] with Bounded[Int] {
    // Overrides
    //
    // Integral
    override val selfIntegral = Integral._ofScalaIntegral[Int]
    // Enum
    private[this] type a = Int
    override val succ: a => a = x => {
        if (x == maxBound) {
            error("Enum[Int].succ: tried to take `succ` of maxBound")
        } else {
            x + 1
        }
    }
    override val pred: a => a = x => {
        if (x == minBound) {
            error("Enum[Int].pred: tried to take `pred` of minBound")
        } else {
            x + 1
        }
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
        if (e1 > e3) {
            Nil
        } else {
            e1 :: enumFromTo(e1 + 1)(e3)
        }
    }
    override val enumFromThenTo: a => a => a => List[a] = e1 => e2 => e3 => {
        val i = e2 - e1
        if (i >= 0) {
            if (e1 > e3) {
                Nil
            } else {
                e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
            }
        } else {
            if (e1 < e3) {
                Nil
            } else {
                e1 :: enumFromThenTo(e1 + i)(e2 + i)(e3)
            }
        }
    }
    // Bounded
    override val minBound: a = JInt.MIN_VALUE
    override val maxBound: a = JInt.MAX_VALUE
}
