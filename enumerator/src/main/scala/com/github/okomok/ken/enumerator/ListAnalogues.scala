

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait ListAnalogues { this: _Enumerator.type =>

    /**
     * Iteratee which cares about the first element only.
     */
    def head[a, n[+_]](implicit im: Monad[n]): Iteratee[a, n, Maybe[a]] = {
        lazy val loop: Stream[a] => Iteratee[a, n, Maybe[a]] = {
            case Chunks(Nil) => head
            case Chunks(x :: xs) => `yield`(Just(x))(Chunks(xs.!))
            case EOF => `yield`(Nothing)(EOF)
        }
        continue(loop)
    }
}
