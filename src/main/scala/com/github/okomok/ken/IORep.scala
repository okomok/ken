

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.tailrec


// type IORep[+a] = RealWorld.type => Product2[a, RealWorld.type]


object IORep {

    // See: scala.util.control.TailCalls
    private[ken] case class Call[+a](rest: IO[a], override val _2: RealWorld.type) extends Product2[a, RealWorld.type] {
        override lazy val _1: a = rest.!
    }
    private[ken] case class Done[+a](override val _1: a, override val _2: RealWorld.type) extends Product2[a, RealWorld.type]

    private[ken] def eval[a](io: IO[a]): a = {
        @tailrec
        def loop(body: IO[a])(s: RealWorld.type): a = body match {
            case IO(rep) => rep(s) match {
                case IORep.Call(rest, s) => loop(rest)(s)
                case IORep.Done(result, s) => result
            }
        }

        loop(io)(RealWorld)
    }
}
