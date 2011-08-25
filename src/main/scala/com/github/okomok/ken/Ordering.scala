

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


sealed abstract class Ordering

case object LT extends Ordering
case object EQ extends Ordering
case object GT extends Ordering


object Ordering extends Bounded[Ordering] with Enum[Ordering] with Monoid[Ordering] with Show[Ordering] with ThisIsInstance {
    // Overrides
    //
    // Bounded
    private type a = Ordering
    override val minBound: a = LT
    override val maxBound: a = GT
    // Enum
    override val succ: a => a = {
        case LT => EQ
        case EQ => GT
        case GT => error("Enum[Ordering].succ: bad argument")
    }
    override val pred: a => a = {
        case GT => EQ
        case EQ => LT
        case LT => error("Enum[Ordering].pred: bad argument")
    }
    override val toEnum: Int => a = {
        case 0 => LT
        case 1 => EQ
        case 2 => GT
    }
    override val fromEnum: a => Int = {
        case LT => 0
        case EQ => 1
        case GT => 2
    }
    override val enumFrom: a => List[a] = Bounded.boundedEnumFrom
    override val enumFromThen: a => a => List[a] = Bounded.boundedEnumFromThen
    // Monoid
    private type m = Ordering
    override val mempty: m = EQ
    override val mappend: m => Lazy[m] => m = x => y => x match {
        case LT => LT
        case EQ => y.!
        case GT => GT
    }
    // Show
    override val showsPrec: Int => a => ShowS = _ => a => Show.showString(a.toString)
}
