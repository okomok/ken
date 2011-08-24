

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ReverseTest extends org.scalatest.junit.JUnit3Suite {
    def testTrivial: Unit = {
        expect(List(7,6,5,4,3,2))(List.reverse(List.range(2, 8)))
    }

    def testRevRev: Unit = {
        expect(List(7,6,5,4,3,2))(List.reverse(List.reverse(List.reverse(List.range(2, 8)))))
    }
}
