

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Id extends Applicative[({ type f[a] = a })#f] {
    private[this] type f[a] = a
    def pure[a](x: => a): f[a] = x
    def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = x(y)
}
