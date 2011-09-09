

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ShowTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val s = List.stringize(show(List(1,2)))
        expect("[1,2]")(s)
    }

    def testNil {
        val s = List.stringize(show(Nil.up))
        expect("[]")(s)
    }

    def testNested {
        val s = List.stringize(show(List(List(1,2), List(3,4,5))))
        expect("[[1,2],[3,4,5]]")(s)
    }

    def testString_1 {
        val s = List.stringize(show(List('a')))
        expect("\"a\"")(s)
    }

    def testString_2 {
        val s = List.stringize(show(List('a','b')))
        expect("\"ab\"")(s)
    }

    def testNestedString_ {
        val s = List.stringize((show(List(List('1','2'), List('3','4','5')))))
        expect("[\"12\",\"345\"]")(s)
    }
}
