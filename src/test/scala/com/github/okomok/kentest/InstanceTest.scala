

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class InstanceTest extends org.scalatest.junit.JUnit3Suite {

    class MyST

    def testTrivial {
        val y/*: State[MyST, List[Int]]*/ = List.I_traverse((a: Int) => State((s: MyST) => (a + 1, s)))(List(1,2,3))
        expect(List(2,3,4))(State.eval(y)(new MyST))
/*
        val z = State.eval(y)(new MyST) // error: cyclic aliasing or subtyping involving type apply1
        expect(List(2,3,4))(z)
*/
    }
}
