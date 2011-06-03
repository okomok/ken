


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TypeableTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val a = "hello"
        val Maybe.Just(x) = Typeable.cast[String, String](a)
        expect(a)(x)
        val c = Typeable.cast[String, Int](a)
        expect(Maybe.Nothing)(c)
    }

    def testParam {
        val a = "hello" :: scala.Nil
        val Maybe.Just(x) = Typeable.cast[scala.List[String], scala.List[String]](a)
        expect(a)(x)
        val c = Typeable.cast[scala.List[String], scala.List[Int]](a)
        expect(Maybe.Nothing)(c)
    }

    def testmkT {
        val x = Typeable.mkT((b: Boolean) => !b)(true)
        expect(false)(x)

        val y = Typeable.mkT((b: Boolean) => !b)('a')
        expect(y)('a')
    }

    def testBadCompiler1 {
        val a = "hello"
        intercept[MatchError] {
            val Maybe.Just(x) = Typeable.cast(a): Maybe[String] // annotation seems ignored.
        }
    }

    def testBadCompiler2 {
        val a = "hello"
        intercept[Error] {
            val r = Typeable.cast(a): Maybe[String]
            r match {
                case Maybe.Just(x) => ()
                case Maybe.Nothing => throw new Error
            }
        }
    }

}
