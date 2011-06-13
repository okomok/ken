

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Unit_ extends Monoid[Unit] {
    private[this] type m = Unit
    override def mempty: m = ()
    override def mappend(x: m)(y: => m): m = ()
    override def mconcat(x: List[m]): m = ()
}
