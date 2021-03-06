

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


class ImplicitPriorityTest extends org.scalatest.junit.JUnit3Suite {

    trait A

    trait B extends A

    trait D extends A

    trait E extends A

    trait _Implicit0  {
        implicit val a: A = new A{}
    }

    trait _Implicit1 extends _Implicit0 {
        implicit def b: B = new B{}
    }

    trait _Implicit2 extends _Implicit1 {
        implicit def d(implicit i: DummyImplicit): D = new D{}
    }

    object _Implicit3 extends _Implicit2 {
        trait Nowhere
        implicit def e(implicit i: Nowhere): E = new E{} // kicked before eligibles overloading resolution.
    }

    def testTrivial {
        import _Implicit3._
        assert(implicitly[A].isInstanceOf[D])
    }

    def testPartialOrdering {
        class Z[a](val b: Boolean)

        implicit def of1[a]: Z[a] = new Z[a](false)
        implicit def of2[a]: Z[List[a]] = new Z[List[a]](true)

        assert(implicitly[Z[List[Int]]].b)
    }
}

