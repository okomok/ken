

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class LazyTest extends org.scalatest.junit.JUnit3Suite {

    def testLazy {
        Lazy(throw new java.lang.Error("wow"))
    }

    def testEval {
        class HasBang {
            def ! : String = "hello"
        }

        class HasEval {
            var called = false
            def _eval: HasEval = { called = true; this }
        }

        object NoBang

        expect("hello")(Lazy(new HasBang).!)
        expect(NoBang)(Lazy(NoBang).!)

        val he = new HasEval
        Lazy(he)._eval
        assert(he.called)
    }
}
