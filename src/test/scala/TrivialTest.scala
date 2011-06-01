

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def testList {
        import ken.List._

        val xs: ftype[Int] = locally {
            ((x: Int) => (y: Int) => x + y) <#> (2 :: 3 :: 4 :: Nil) <*> pure(4)
        }
        expect(6 :: 7 :: 8 :: Nil)(xs)

        expect(true)(Nil == Nil)
        expect(false)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: Nil)
        expect(true)(6 :: 7 :: 8 :: Nil == 6 :: 7 :: 8 :: Nil)
        expect(false)(6 :: 7 :: Nil == 6 :: 7 :: 8 :: Nil)

        def makeList: ftype[Int] = throw new Error
        val ys = ::(10, ken.Lazy(makeList))
        expect(false)(6 :: 7 :: Nil == ys)

        /*
        def callfmap[x <: ken.Functor, a, b](x: x)(y: a => b)(z: f[a]): f[b] = x.fmap(y)(z)

        callfmap(ken.List_)((x: Int) => x)(3 :: Nil)
        */

    }

    def compileImplicit {
        import ken.List.Nil

        (1 :: 2 :: Nil) >>= (x => x :: Nil)

        def takeImplicit[f[_], a](x: f[a])(implicit i: ken.Monad[f]): Unit = throw new Error
        def takeMonoidImplicit[a](x: a)(implicit i: ken.Monoid[a]): Unit = throw new Error

        val u1: Unit = takeImplicit(6 :: Nil)
        val u2: Unit = takeMonoidImplicit(6 :: Nil)
    }

    def teztIO {
        import ken.Prelude
   //     val io = Prelude.getChar >>= { x => Prelude.putChar(x) }
   //     io.unIO()

        val io = for {
            x <- Prelude.getChar
            u <- Prelude.putChar(x)
        } yield u

        io.unIO()
    }

    def testStreamDefect {
        def makeStream: Stream[Int] = throw new Error
        val xs: Stream[Int] = Stream.cons(1, makeStream)

        //import ken.#::

        intercept[Error] {
            xs match {
                case Stream.Empty => ()
                case x #:: xs => ()
            }
        }
    }

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
