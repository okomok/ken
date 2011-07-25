

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


class ImplicitPriorityTest extends org.scalatest.junit.JUnit3Suite {

    trait A

    trait B extends A

    trait D extends A

    trait _Implicit0  {
        implicit val a: A = new A{}
    }

    trait _Implicit1 extends _Implicit0 {
        implicit val b: B = new B{}
    }

    object _Implicit2 extends _Implicit1 {
        implicit val d: D = new D{}
    }

    def testTrivial {
        import _Implicit2._
        assert(implicitly[A].isInstanceOf[D])
    }
}
