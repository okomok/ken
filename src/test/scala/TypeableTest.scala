


// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TypeableTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val a = "hello"
        val Just(x) = Typeable.cast[String, String](a)
        expect(a)(x)
        val c = Typeable.cast[String, Int](a)
        expect(Nothing)(c)
    }

    def testParam {
        val a = "hello" :: scala.Nil
        val Just(x) = Typeable.cast[scala.List[String], scala.List[String]](a)
        expect(a)(x)
        val c = Typeable.cast[scala.List[String], scala.List[Int]](a)
        expect(Nothing)(c)
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
            val Just(x) = Typeable.cast(a): Maybe[String] // annotation seems ignored.
        }
    }

    def testBadCompiler2 {
        val a = "hello"
        intercept[Error] {
            val r = Typeable.cast(a): Maybe[String]
            r match {
                case Just(x) => ()
                case Nothing => throw new Error
            }
        }
    }

    def testmkQ {
        val x = Typeable.mkQ(22)((c: Char) => c.toInt)('a')
        expect(97)(x)

        val y = Typeable.mkQ(22)((c: Char) => c.toInt)('b')
        expect(98)(y)

        val z = Typeable.mkQ(22)((c: Char) => c.toInt)(true)
        expect(22)(z)
    }

}
