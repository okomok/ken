

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait Types { this: Enumerator.type =>
    def runIteratee[a, n[+_], b](m: Iteratee[a, n, b]): n[Step[a, n, b]] = m.run

    def returnI[a, n[+_], b](s: Step[a, n, b])(implicit i: Monad[n]): Iteratee[a, n, b] = Iteratee { i.`return`(s) }
    def `yield`[a, n[+_], b](x: b)(extra: Stream[a])(implicit i: Monad[n]): Iteratee[a, n, b] = returnI(Yield(x, extra))
    def continue[a, n[+_], b](k: Stream[a] => Iteratee[a, n, b])(implicit i: Monad[n]): Iteratee[a, n, b] = returnI(Continue(k))
}
