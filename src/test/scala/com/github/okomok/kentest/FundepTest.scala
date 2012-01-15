

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class FundepTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val _M = MonadError[Error.apply[String]]
        def foo[m[+a], a, e](m: m[a])(implicit _M: MonadError.Of[e, m], _E: ErrorClass[e]) = m
        foo(_M.`return`(10))
    }

}
