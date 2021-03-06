

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def testList {
        val xs: List[Int] = locally {
            import List._
            ((x: Int) => (y: Int) => x + y) <@> (2 :: 3 :: 4 :: Nil) <*> pure(4)
        }
        expect(6 :: 7 :: 8 :: Nil)(xs)

        expect(true)(Nil == Nil)
        expect(false)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: Nil)
        expect(true)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: 8 :: Nil)
        expect(false)(6 :: 7 :: Nil == 6 :: 7 :: 8 :: Nil)

        def makeList: List[Int] = undefined
        val ys = ::(10, Lazy(makeList))
        expect(false)(6 :: 7 :: Nil == ys)
    }
/*
    def testLazy {
        class Foo {
            def eval: Int = 3
        }
        val x = Lazy(new Foo)
        expect(3)(x.eval)
    }
*/
    def compileImplicit {
        (1 :: 2 :: Nil) >>= (x => x :: Nil)

        def takeImplicit[f[+_], a](x: f[a])(implicit i: Monad[f]): Unit = undefined
        def takeMonoidImplicit[a](x: a)(implicit i: Monoid[a]): Unit = undefined

        val u1: Unit = takeImplicit(6 :: Nil)
        val u2: Unit = takeMonoidImplicit(6 :: Nil)
    }

    def teztIO {
        val io = for {
            x <- IO.getChar
            u <- IO.putChar(x)
        } yield u

        io.!
    }

    def teztIOAp {
        import IO._

        val io = { (c1: Char) => (c2: Char) => println(c1); println(c2) } <@> getChar <*> getChar
        io.!
    }

    /*
    def testStreamDefect {
        import
        def makeStream: Stream[Int] = throw new Error
        val xs: Stream[Int] = Stream.cons(1, makeStream)

        intercept[Error] {
            xs match {
                case Stream.Empty => ()
                case x #:: xs => ()
            }
        }
    }
    */

    /*
    def testImport {
        object xxx {
            type L = Int
            object yyy {
                type L = xxx.L
            }
        }
        import xxx._
        import yyy._
        val x: L = 3
    }
    */
}
