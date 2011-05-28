

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken
package detail


private[ken]
class Function1Instance[c] extends Applicative[({type f[a] = c => a})#f] {
    // Functor
    override def fmap[a, b](x: a => b)(y : c => a): c => b = z => x(y(z))
    // Applicative
    override def pure[a](x: => a): c => a = const(x)
    override def op_<*>[a, b](x: c => a => b)(y: c => a): c => b = z => x(z)(y(z))
}
