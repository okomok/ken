

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


object Unit extends Bounded[Unit] with Enum[Unit] with Eq.Of[Unit]
    with Ix[Unit] with Monoid[Unit] with Ord[Unit] with Show[Unit]
{
    // Overrides
    //
    // Bounded
    override val minBound: minBound = ()
    override val maxBound: maxBound = ()
    // Enum
    override val succ: succ = _ => error("Enum[Unit].succ: bad argument")
    override val pred: pred = _ => error("Enum[Unit].pred: bad argument")
    override val toEnum: toEnum = x => {
        if (x == 0) ()
        else error("Enum[Unit].toEnum: bad argument")
    }
    override val fromEnum: fromEnum = _ => 0
    override val enumFrom: enumFrom = _ => List(())
    override val enumFromThen: enumFromThen = _ => _ => List.repeat(())
    override val enumFromTo: enumFromTo = _ => _ => List(())
    override val enumFromThenTo: enumFromThenTo = _ => _ => _ => List.repeat(())
    // Ix
    override val range: range = _ => List(())
    override val unsafeIndex: unsafeIndex = _ => _ => 0
    override val inRange: inRange = _ => _ => True
    override val index: index = unsafeIndex
    // Ord
    override val compare: compare = _ => _ => EQ
    override val op_< : op_< = _ => _ => False
    override val op_<= : op_<= = _ => _ => True
    override val op_> : op_> = _ => _ => False
    override val op_>= : op_>= = _ => _ => True
    override val max: max = _ => _ => ()
    override val min: min = _ => _ => ()
    // Monoid
    override val mempty: mempty = ()
    override val mappend: mappend = _ => _ => ()
    override val mconcat: mconcat = _ => ()
    // Show
    override val showsPrec: showsPrec = _ => a => Show.showString(a.toString)
}
