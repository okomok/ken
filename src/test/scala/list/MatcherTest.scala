

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class MatcherTest extends org.scalatest.junit.JUnit3Suite {

    def testLazy: Unit = {
        val x :: xs = 1 !:: 2 !:: 3 !:: 4 !:: 5 !:: Nil
        expect(1)(x)
        expect(2 !:: 3 !:: 4 !:: 5 !:: Nil)(xs!)
    }

    def testStrict: Unit = {
        val x !:: y !:: ys = 1 !:: 2 !:: 3 !:: 4 !:: 5 !:: Nil
        expect(1)(x)
        expect(2)(y)
        expect(3 !:: 4 !:: 5 !:: Nil)(ys)
    }

    def testJumble: Unit = {
        val x !:: y !:: (z :: zs) = 1 !:: 2 !:: 3 !:: 4 !:: 5 !:: Nil
        expect(1)(x)
        expect(2)(y)
        expect(3)(z)
        expect(4 !:: 5 !:: Nil)(zs!)
    }

}
