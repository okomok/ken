

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object ZipList extends Applicative[List] {
    private[this] type f[a] = List[a]
    override def pure[a](x: => a): f[a] = List.repeat(x)
    override def op_<*>[a, b](x: f[a => b])(y: f[a]): f[b] = List.zipWith[a => b, a, b](op_@)(x)(y)
}
