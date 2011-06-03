

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def testList {
        import Applicative._

        val xs: List[Int] = locally {
            implicit val i = implicitly[Applicative[List]]
            ((x: Int) => (y: Int) => x + y) <#> (2 :: 3 :: 4 :: Nil) <*> pure(4)(i)
        }
        expect(6 :: 7 :: 8 :: Nil)(xs)

        expect(true)(Nil == Nil)
        expect(false)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: Nil)
        expect(true)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: 8 :: Nil)
        expect(false)(6 :: 7 :: Nil == 6 :: 7 :: 8 :: Nil)

        def makeList: List[Int] = throw new Error
        val ys = ::(10, Lazy(makeList))
        expect(false)(6 :: 7 :: Nil == ys)
    }

    def compileImplicit {
        import Monad.>>=
        (1 :: 2 :: Nil) >>= (x => x :: Nil)

        def takeImplicit[f[_], a](x: f[a])(implicit i: Monad[f]): Unit = throw new Error
        def takeMonoidImplicit[a](x: a)(implicit i: Monoid[a]): Unit = throw new Error

        val u1: Unit = takeImplicit(6 :: Nil)
        val u2: Unit = takeMonoidImplicit(6 :: Nil)
    }

    def teztIO {
        import Monad.`for`
   //     val io = getChar >>= { x => putChar(x) }
   //     io.unIO()

        val io = for {
            x <- getChar
            u <- putChar(x)
        } yield u

        io.unIO()
    }

    def teztIOAp {
        import Applicative._

        val io = { (c1: Char) => (c2: Char) => println(c1); println(c2) } <#> getChar <*> getChar
        io.unIO()
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
