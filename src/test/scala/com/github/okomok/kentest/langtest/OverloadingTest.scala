

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest.langtest


// See: https://issues.scala-lang.org/browse/SI-2551


class OverloadingTest extends org.scalatest.junit.JUnit3Suite {

    trait A

    object X {
        implicit def foo1: A = new A{} // needs different name...
    }

    trait B

    object Y {
        implicit def foo: B = new B{}
    }

    def testTrivial {
        import X._
        import Y._

        implicitly[A]
        implicitly[B]
        ()
    }
}
