


// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TypeableTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val a = "hello"
        val Just(x) = Typeable.cast(a, Type[Predef.String])
        expect(a)(x)
        val c = Typeable.cast(a, Type[Int])
        expect(Nothing)(c)
    }

    def testParam {
        val a = "hello" :: scala.Nil
        val Just(x) = Typeable.cast(a, Type[scala.List[Predef.String]])
        expect(a)(x)
        val c = Typeable.cast(a, Type[scala.List[Int]])
        expect(Nothing)(c)
    }

    def testmkT {
        val x = Typeable.mkT((b: Boolean) => !b)(true)
        expect(false)(x)

        val y = Typeable.mkT((b: Boolean) => !b)('a')
        expect(y)('a')
    }
/*
    def testBadCompiler1 {
        val a = "hello"
        intercept[MatchError] {
            val Just(x) = Typeable.cast(a): Maybe[Predef.String] // annotation seems ignored.
        }
    }

    def testBadCompiler0 {
        val a = "hello"
        intercept[Error] {
            val r = identity[Maybe[Predef.String]](Typeable.cast(a))
            r match {
                case Just(x) => ()
                case Nothing => throw new Error
            }
        }
    }

    def testBadCompiler2 {
        val a = "hello"
        intercept[Error] {
            val r = Typeable.cast(a): Maybe[Predef.String]
            r match {
                case Just(x) => ()
                case Nothing => throw new Error
            }
        }
    }
*/
    def testmkQ {
        val x = Typeable.mkQ(22)((c: Char) => c.toInt)('a')
        expect(97)(x)

        val y = Typeable.mkQ(22)((c: Char) => c.toInt)('b')
        expect(98)(y)

        val z = Typeable.mkQ(22)((c: Char) => c.toInt)(true)
        expect(22)(z)
    }

}
