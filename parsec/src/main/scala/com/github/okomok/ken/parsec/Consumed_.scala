

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


sealed abstract class Consumed_[+a] extends Up[Consumed_[a]]

final case class Consumed[+a](reply: a) extends Consumed_[a]
final case class Empty[+a](reply: a) extends Consumed_[a]


object Consumed_ extends Functor[Consumed_] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private type f[+a] = Consumed_[a]
    override def fmap[a, b](f: a => b): f[a] => f[b] = {
        case Consumed(x) => Consumed(f(x))
        case Empty(x) => Empty(f(x))
    }
}
