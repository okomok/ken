

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class CycleTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val L = List.cycle(1 :: 2 :: 3 :: Nil)
        val A = 1 :: 2 :: 3 :: 1 :: 2 :: 3 :: 1 :: 2 :: Nil
        expect(A)(List.take(8)(L))
        expect(A)(List.take(8)(L))
    }
}
