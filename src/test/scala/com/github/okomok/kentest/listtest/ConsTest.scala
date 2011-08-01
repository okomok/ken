

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ConsTest extends org.scalatest.junit.JUnit3Suite {

    def testInfer: Unit = {
        val x: List[Int] = 1 :: 3 :: 4 :: Nil
        expect(10)(x.!!(1) + 7)
    }

    def testInferStrict: Unit = {
        val x: List[Int] = 1 !:: 3 !:: 4 !:: Nil
        expect(10)(x.!!(1) + 7)
    }

}
