

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ShowTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val s = List.toJString(Show.show(List(1,2)))
        expect("List(1,2)")(s)
    }

    def testListNothing {
        // hmmmm....
        //val s = List.toJString(Show.show(Nil.up))
        //expect("Nil")(s)
    }

    def testNilType {
        val s = List.toJString(Show.show(Nil))
        expect("Nil")(s)
    }

    def testNested {
        val s = List.toJString(Show.show(List(List(1,2), List(3,4,5))))
        expect("List(List(1,2),List(3,4,5))")(s)
    }

    def testString_1 {
        val s = List.toJString(Show.show(List('a')))
        expect("\"a\"")(s)
    }

    def testString_2 {
        val s = List.toJString(Show.show(List('a','b')))
        expect("\"ab\"")(s)
    }

    def testNestedString_ {
        val s = List.toJString((Show.show(List(List('1','2'), List('3','4','5')))))
        expect("List(\"12\",\"345\")")(s)
    }
}
