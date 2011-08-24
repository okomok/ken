

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class IntTest extends org.scalatest.junit.JUnit3Suite {

    def testEnumFrom {
        val xs = Int.enumFrom(10)
        expect(List(10,11,12))(List.take(3)(xs))
    }

    def testEnumFromThen {
        val xs = Int.enumFromThen(10)(11)
        expect(List(10,11,12))(List.take(3)(xs))
    }

    def testEnumFromThenStep {
        val xs = Int.enumFromThen(10)(15)
        expect(List(10,15,20,25,30))(List.take(5)(xs))
    }

    def testEnumFromThenStepReduce {
        val xs = Int.enumFromThen(10)(8)
        expect(List(10,8,6,4))(List.take(4)(xs))
    }

    def testEnumFromTo {
        val xs = Int.enumFromTo(10)(13)
        expect(List(10,11,12,13))(xs)
    }

    def testEnumFromToReduce {
        val xs = Int.enumFromTo(13)(10)
        expect(Nil)(xs)
    }

    def testEnumFromThenToStep {
        val xs = Int.enumFromThenTo(10)(15)(28)
        expect(List(10,15,20,25))(xs)
    }

    def testEnumFromThenToStepNil {
        val xs = Int.enumFromThenTo(10)(15)(2)
        expect(Nil)(xs)
    }

    def testEnumFromThenToStepReduce {
        val xs = Int.enumFromThenTo(15)(12)(8)
        expect(List(15,12,9))(xs)
    }

    def testEnumFromThenToStepReduceNil {
        val xs = Int.enumFromThenTo(15)(12)(16)
        expect(Nil)(xs)
    }

    def testEnumFromThenToStepReduceOne {
        val xs = Int.enumFromThenTo(15)(12)(15)
        expect(List(15))(xs)
    }
}
