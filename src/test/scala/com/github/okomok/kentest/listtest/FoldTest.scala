

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class FoldTest extends org.scalatest.junit.JUnit3Suite {

    def testRight: Unit = {
        val A: List[Int] = List.cycle(1 :: 3 :: 4 :: Nil) // infinite
        val B: List[Int] = List.foldr(List.op_::[Int])(Nil.of[Int])(A)
        expect(List.take(30)(A))(List.take(30)(B))
    }
}
