

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Unit extends Bounded[Unit] with Enum[Unit] with Monoid[Unit] with Show[Unit] {
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
    override val fromEnum: a => Int = { case () => 0 }
    override val enumFrom: a => List[a] = { case () => List(()) }
    override def enumFromThen: a => a => List[a] = { case () => { case () => List.repeat(()) } }
    override def enumFromTo: a => a => List[a] = { case () => { case () => List(()) } }
    override def enumFromThenTo: a => a => a => List[a] = { case () => { case () => { case () => List.repeat(()) } } }
    // Monoid
    private type m = Unit
    override val mempty: m = ()
    override val mappend: m => Lazy[m] => m = { _ => _ => () }
    override val mconcat: List[m] => m = { _ => () }
    // Show
    override val showsPrec: Int => a => ShowS = _ => a => Show.showString(a.toString)
}
