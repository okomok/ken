

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class NullTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        val tr = List.empty.of[Int]
        assert(List.`null`(tr))
        assert(List.`null`(tr))
    }
}
