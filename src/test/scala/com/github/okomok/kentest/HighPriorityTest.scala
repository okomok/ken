

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class HighPriorityTest extends org.scalatest.junit.JUnit3Suite {

    trait A

    trait B extends A

    trait _Implicit0  {
        implicit def b: B = new B{}
    }

    object _Implicit1 extends _Implicit0 {
        // implicit def a: A = new A{} // makes it ambiguous.
        implicit def a: A with HighPriority = new A with HighPriority{}
    }

    def testTrivial {
        import _Implicit1._
        expect(false)(implicitly[A].isInstanceOf[B])
    }
}

