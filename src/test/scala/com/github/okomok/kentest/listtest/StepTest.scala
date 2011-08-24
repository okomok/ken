

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class StepTest extends org.scalatest.junit.JUnit3Suite {
/* REJECTED
    def testStep0: Unit = {
        // Unlike Vector, 0 is allowed.
        val tr = List(1,2,3,4,5,6).toList.step(0)
        expect(List(1,1,1,1,1))(tr.take(5))
        expect(List(1,1,1,1,1))(tr.take(5))
    }
*/
    def testStep1: Unit = {
        val tr = List.step(1)(List(1,2,3,4,5,6))
        expect(List(1,2,3,4,5))(List.take(5)(tr))
        expect(List(1,2,3,4,5))(List.take(5)(tr))
    }

    def testStep2: Unit = {
        val tr = List.step(2)(List(1,2,3,4,5,6))
        expect(tr)(List(1,3,5))
    }

    def testStep3: Unit = {
        val tr = List.step(3)(List(1,2,3,4,5,6))
        expect(tr)(List(1,4))
    }

    def testStepStep: Unit = {
        val tr = List.step(2)(List.step(3)(List(1,2,3,4,5,6,7,8,9,10,11)))
        expect(tr)(List(1,7))
    }

    def testStepEmpty: Unit = {
//        val tr0 = Nil.of[Int].step(0)
//        assert(tr0.isEmpty)
        val tr1 = List.step(1)(Nil.of[Int])
        assert(List.`null`(tr1))
        val tr2 = List.step(2)(Nil.of[Int])
        assert(List.`null`(tr2))
    }

}
