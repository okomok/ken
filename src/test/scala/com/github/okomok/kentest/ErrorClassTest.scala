

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ErrorClassTest extends org.scalatest.junit.JUnit3Suite {

    def testInstance {
        val i = ErrorClass[Kind.const[NoSuchMethodException]]
        val x = i.strMsg("wow")
        assert(x.isInstanceOf[NoSuchMethodException])
        expect("wow")(x.getMessage)
    }
}
