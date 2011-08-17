

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class LazyTest extends org.scalatest.junit.JUnit3Suite {

    def testEval {
        class HasBang {
            def ! : String = "hello"
        }

        object NoBang

        expect("hello")(Lazy(new HasBang).!)
        expect(NoBang)(Lazy(NoBang).!)
    }
}
