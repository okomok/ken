

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class InstanceTest extends org.scalatest.junit.JUnit3Suite {

    class MyST

    def testTrivial {
        val tr = Traversable[List.type]
        val y = tr.traverseI((a: Int) => State((s: MyST) => (a + 1, s)))(List(1,2,3)) // implicit lookup ok!
        val _y = y.asInstanceOf[ State[MyST, List[MyST]] ]                            // but type of `y` is invisible....
        val z = State.eval(_y)(new MyST)
        expect(List(2,3,4))(z)
    }

}
