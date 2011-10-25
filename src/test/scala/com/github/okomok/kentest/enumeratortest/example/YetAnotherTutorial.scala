

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.enumeratortest.example


import com.github.okomok.ken._
import enumerator._
import Enumerator._


class YetAnotherTutorialTest extends org.scalatest.junit.JUnit3Suite {

    def sum6[m[+_]](implicit in: Monad[m]): Iteratee[Int, m, Int] = {
        val itm = Monad[Iteratee.apply2[Int, m]]
        import itm._
        for {
            maybeNum <- head[Int, m]
        } {
            maybeNum match {
                case Nothing => `return`(0)
                case Just(i) => for {
                    rest <- sum6
                } {
                    `return` { i + rest }
                }
            }
        }
    }

    def sum8[m[+_]](implicit in: Monad[m]): Iteratee[Int, m, Int] = Iteratee {
        import in._
        for {
            step <- runIteratee(sum6)
        } {
            step match {
                case Continue(k) => runIteratee { k { Chunks(List.range(1, 11)) } }
                case _ => `return`(step)
            }
        }
    }

    def testTrivial8 {
        implicit val iom = Monad[Identity.type]
        val v = run_(sum8)
        expect(Identity(55))(v)
    }

    def testTrivial9 {
        implicit val idm = Monad[Identity.type]
        val v = run_(sum6 >>== enumList(3)(List.range(1, 11)))
        expect(Identity(55))(v)
    }

    def testTrivial {
        implicit val iom = Monad[IO.type]
        import IO.>>=
        val io = run_(sum8) >>= (x => IO.print(x))
        io.!
    }
}
