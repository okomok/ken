

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


// See: https://gist.github.com/1022916


class ArrowLoopTest extends org.scalatest.junit.JUnit3Suite {

    val ar = ArrowLoop[Function.type]

    val factorial = ar.loop[Int, Int, Int => Int] {
        case (b, d) => (
            Lazy { d.!(b) },
            Lazy {
                case 0 => 1
                case x => x * d.!(x - 1)
            }
        )
    }

    def testTrivial {
        expect(List(1,1,2,6,24,120,720,5040,40320,362880))(List.map(factorial)(List.range(0, 10)))
    }
}
