

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Functor[f[_]] {
    type ftype[a] = f[a]
    def fmap[a, b](x: a => b)(y: f[a]): f[b]
}


object Functor extends ApplicativeInstance
