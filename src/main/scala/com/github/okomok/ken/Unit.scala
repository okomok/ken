

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Unit extends Monoid[Unit] with ThisIsInstance {
    // Overrides
    //
    // Monoid
    private[this] type m = Unit
    override val mempty: m = ()
    override val mappend: m => (=> m) => m = { _ => _ => () }
    override val mconcat: List[m] => m = { _ => () }
}
