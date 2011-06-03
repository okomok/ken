


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class TypeableTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val ac = implicitly[ken.Typeable[String]]
        val bc = implicitly[ken.Typeable[String]]
        val a = "hello"
        val ken.Maybe.Just(x) = ac.cast[String](a)
        expect(a)(x)
        val c: ken.Maybe[Int] = ac.cast(a)
        expect(ken.Maybe.Nothing)(c)
    }

    def testParam {
        val ac = implicitly[ken.Typeable[scala.List[String]]]
        val bc = implicitly[ken.Typeable[scala.List[String]]]
        val a = "hello" :: scala.Nil
        val ken.Maybe.Just(x) = ac.cast[scala.List[String]](a)
        expect(a)(x)
        val c = ac.cast[scala.List[Int]](a)
        expect(ken.Maybe.Nothing)(c)
    }

}
