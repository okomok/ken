

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid {
    type a_
    def mempty: a_
    def mappend(x: a_)(y: a_): a_
    def mconcat(x: List[a_]): a_ = x.foldRight(mempty)(mappend(_)(_))

    private[ken] class _Mappend_(x: a_) {
        def _mappend_(y: a_): a_ = mappend(x)(y)
    }
    implicit def _mappend_(x: a_): _Mappend_ = new _Mappend_(x)
}
