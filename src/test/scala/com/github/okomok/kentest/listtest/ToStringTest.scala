

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ToStringTest extends org.scalatest.junit.JUnit3Suite {

    def testNil {
        expect(scala.Nil.toString)(Nil.toString)
    }

    def testNonChar {
        expect(scala.List(1,2,3).toString)(List(1,2,3).toString)
    }

    def testChar {
        // rejected
        // expect("\"123\"")(List('1','2','3').toString)
    }

    def testEllipse {
        //println(List.repeat(3))
    }
}
