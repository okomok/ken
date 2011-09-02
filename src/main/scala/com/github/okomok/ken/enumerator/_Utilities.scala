

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2010 John Millikin
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package enumerator


private[enumerator] trait _Utilities[n[+_]] { this: _Enumerators[n] =>

    // Unsorted Utilities
    //
    def enumEOF[a, b]: Enumerator[a, b] = {
        case Yield(x, _) => `yield`(x)(EOF)
        case Error(err) => throwError(err)
        case Continue(k) => {
            def check(s: Step[a, b]): Iteratee[a, b] = s match {
                case Continue(_) => error("enumEOF: divergent iteratee")
                case s => enumEOF(s)
            }
            k(EOF) >>== check
        }
    }

    // Utilities for testing and debugging
    //
    def enumList[a, b](n: Integer)(xs: List[a], * : Type[b] = null): Enumerator[a, b] = {
        def loop(xs: List[a])(step: Step[a, b]): Iteratee[a, b] = step match {
            case Continue(k) if not(List.`null`(xs)) => {
                val (s1, s2) = List.genericSplitAt(n)(xs)
                k(Chunks(s1)) >>== loop(s2)
            }
            case _ => returnI(step)
        }
        loop(xs)
    }
}
