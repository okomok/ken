

// Copyright Shunsuke Sogame 2011.
//
// (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package parsec


sealed abstract class Consumed_[+a] extends Up[Consumed_[a]]

final case class Consumed[+a](_1: a) extends Consumed_[a]
final case class Empty[+a](_2: a) extends Consumed_[a]


object Consumed_ extends Functor[Consumed_] {
    // Overrides
    //
    // Functor
    private type f[+a] = Consumed_[a]
    override def fmap[a, b](f: a => b)(x: f[a]): f[b] = x match {
        case Consumed(x) => Consumed(f(x))
        case Empty(x) => Empty(f(x))
    }
}