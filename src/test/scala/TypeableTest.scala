


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class TypeableTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val ac = implicitly[ken.Typeable[String]]
        val bc = implicitly[ken.Typeable[String]]
        val a = "hello"
        val b = ac.cast[String](a)
        b match {
            case ken.Maybe.Just(x) => expect(a)(x)
            case _ => fail("doh")
        }
        val c: ken.Maybe[Int] = ac.cast(a)
        expect(ken.Maybe.Nothing)(c)
    }

}
