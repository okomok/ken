

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Unit_ extends Monoid {
    override type m_ = Unit
    override def mempty: m_ = ()
    override def mappend(x: m_)(y: m_): m_ = ()
    override def mconcat(x: List[m_]): m_ = ()
}
