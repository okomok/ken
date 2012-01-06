

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class BooleanTest extends org.scalatest.junit.JUnit3Suite {
    def testShortCircuit: Unit = {
        val L = (true !:: true !:: false !:: true !:: Nil) ++: List.cycle(true !:: Nil)
        expect(false)(List.and(L))
        assert(List.or(L))
    }
}
