

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.tailrec


// type IORep[+a] = RealWorld.type => Product2[a, RealWorld.type]


object IORep {

    // See: scala.util.control.TailCalls (`rest` may be just `IORep[a]` in fact.)
    private[ken] case class Call[+a](rest: () => IORep[a], override val _2: RealWorld.type) extends Product2[a, RealWorld.type] {
        override lazy val _1: a = eval(rest())
    }
    private[ken] case class Done[+a](override val _1: a, override val _2: RealWorld.type) extends Product2[a, RealWorld.type]

    private[ken] def eval[a](rep: IORep[a]): a = {
        @tailrec
        def loop(rep: IORep[a])(s: RealWorld.type): a = rep(s) match {
            case Call(rest, s) => loop(rest())(s)
            case Done(result, s) => result
        }

        loop(rep)(RealWorld)
    }
}
