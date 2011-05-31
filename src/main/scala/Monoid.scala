

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid[m] {
    type mtype = m
    def mempty: m
    def mappend(x: m)(y: m): m
    def mconcat(x: List[m]): m = Prelude.foldr[m, m](x => y => mappend(x)(y()))(mempty)(x)

    private[ken] class Mappend_(x: m) {
        def _mappend_(y: m): m = mappend(x)(y)
    }
    implicit def _mappend_(x: m): Mappend_ = new Mappend_(x)
}
