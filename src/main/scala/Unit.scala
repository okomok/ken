

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object TheUnit extends Monoid {
    override type a_ = Unit
    override def mempty: a_ = ()
    override def mappend(x: a_)(y: a_): a_ = ()
    override def mconcat(x: List[a_]): a_ = ()
}
