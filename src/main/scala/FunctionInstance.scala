

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


class Function1Instance[z] extends Functor {
    override type f_[a] = z => a
    override def fmap[a, b](x: a => b)(y : f_[a]): f_[b] = z => x(y(z))
}
