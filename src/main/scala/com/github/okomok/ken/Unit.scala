

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


object Unit extends Monoid[Unit] with Bounded[Unit] {
    // Overrides
    //
    // Monoid
    private type m = Unit
    override val mempty: m = ()
    override val mappend: m => Lazy[m] => m = { _ => _ => () }
    override val mconcat: List[m] => m = { _ => () }
    // Bounded
    private type a = Unit
    override val minBound: a = ()
    override val maxBound: a = ()
}
