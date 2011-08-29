

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


object Unit extends Bounded[Unit] with Enum[Unit] with Eq.Of[Unit] with Monoid[Unit] with Ord[Unit] with Show[Unit] {
    // Overrides
    //
    // Bounded
    private type a = Unit
    override val minBound: a = ()
    override val maxBound: a = ()
    // Enum
    override val succ: a => a = _ => error("Enum[Unit].succ: bad argument")
    override val pred: a => a = _ => error("Enum[Unit].pred: bad argument")
    override val toEnum: Int => a = x => {
        if (x == 0) ()
        else error("Enum[Unit].toEnum: bad argument")
    }
    override val fromEnum: a => Int = _ => 0
    override val enumFrom: a => List[a] = _ => List(())
    override val enumFromThen: a => a => List[a] = _ => _ => List.repeat(())
    override val enumFromTo: a => a => List[a] = _ => _ => List(())
    override val enumFromThenTo: a => a => a => List[a] = _ => _ => _ => List.repeat(())
    // Ord
    override val compare: a => a => Ordering = _ => _ => EQ
    override val op_< : a => a => Bool = _ => _ => False
    override val op_<= : a => a => Bool = _ => _ => True
    override val op_> : a => a => Bool = _ => _ => False
    override val op_>= : a => a => Bool = _ => _ => True
    override val max: a => a => a = _ => _ => ()
    override val min: a => a => a = _ => _ => ()
    // Monoid
    private type m = Unit
    override val mempty: m = ()
    override val mappend: m => Lazy[m] => m = _ => _ => ()
    override val mconcat: List[m] => m = _ => ()
    // Show
    override val showsPrec: Int => a => ShowS = _ => a => Show.showString(a.toString)
}
