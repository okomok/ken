

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait _ListAnalogues[n[+_]] { this: _Enumerators[n] =>
    def head[a]: Iteratee[a, Maybe[a]] = {
        def loop(in: Stream[a]): Iteratee[a, Maybe[a]] = in match {
            case Chunks(Nil) => head
            case Chunks(x :: xs) => `yield`(Just(x))(Chunks(xs.!))
            case EOF => `yield`(Nothing)(EOF)
        }
        continue(loop)
    }
}
