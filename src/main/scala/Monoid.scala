

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Monoid {
    type m_
    def mempty: m_
    def mappend(x: m_)(y: m_): m_
    def mconcat(x: List[m_]): m_ = x.foldRight(mempty)(mappend(_)(_))

    private[ken] class Mappend_(x: m_) {
        def _mappend_(y: m_): m_ = mappend(x)(y)
    }
    implicit def _mappend_(x: m_): Mappend_ = new Mappend_(x)
}
