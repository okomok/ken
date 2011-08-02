

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Unit extends Monoid[Unit] {
    // Overrides
    //
    // Monoid
    private[this] type m = Unit
    override val mempty: m = ()
    override val mappend: m => (=> m) => m = { _ => _ => () }
    override val mconcat: List[m] => m = { _ => () }

    // Instances
    //
    implicit val monoid: Monoid[Unit] = this
}
