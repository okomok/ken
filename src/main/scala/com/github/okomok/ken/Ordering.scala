

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


object Ordering extends Bounded[Ordering] with Enum[Ordering] with Ix[Ordering]
    with Monoid[Ordering] with ThisIsInstance
{
    // Overrides
    //
    // Bounded
    override val minBound: minBound = LT
    override val maxBound: maxBound = GT
    // Enum
    override val succ: succ = {
        case LT => EQ
        case EQ => GT
        case GT => error("Enum[Ordering].succ: bad argument")
    }
    override val pred: pred = {
        case GT => EQ
        case EQ => LT
        case LT => error("Enum[Ordering].pred: bad argument")
    }
    override val toEnum: toEnum = {
        case 0 => LT
        case 1 => EQ
        case 2 => GT
    }
    override val fromEnum: fromEnum = {
        case LT => 0
        case EQ => 1
        case GT => 2
    }
    override val enumFrom: enumFrom = Bounded.boundedEnumFrom
    override val enumFromThen: enumFromThen = Bounded.boundedEnumFromThen
    // Ix
    override val range: range = { case (m, n) => enumFromTo(m)(n) }
    override val unsafeIndex: unsafeIndex = { case (l, _) => i => fromEnum(i) - fromEnum(l) }
    override val index: index = b => i => {
        if (inRange(b)(i)) unsafeIndex(b)(i)
        else indexError(b)(i)("Bool")
    }
    override val inRange: inRange = { case (l, u) => i => fromEnum(i) >= fromEnum(l) && fromEnum(i) <= fromEnum(u) }
    // Monoid
    override val mempty: mempty = EQ
    override val mappend: mappend = x => y => x match {
        case LT => LT
        case EQ => y.!
        case GT => GT
    }
}
