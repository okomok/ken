

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class SemigroupTest extends org.scalatest.junit.JUnit3Suite {

    def testMin {
        val m = Semigroup.Min(List.from("hello"))
        val i = Semigroup[Semigroup.Min[String]]
        import i._
        m <>: m
    }

    def testMinMonoid {
        val m = Semigroup.Min(10)
        val i = Monoid[Semigroup.Min[Int]]
        import i._
        m <>: m
        m _mappend_ m
    }
}
