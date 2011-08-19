

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest.enumeratortest.example


import com.github.okomok.ken._


class YetAnotherTutorialTest extends org.scalatest.junit.JUnit3Suite {

    final class My[m <: Kind.Function1](override implicit val inner: Monad[m#apply]) extends EnumeratorsOf[m] {
        val im = Monad[Iteratee.apply[Int]]

        def sum6: Iteratee[Int, Int] = {
            import im.forComp
            for {
                maybeNum <- head[Int]
                * <- maybeNum match {
                    case Nothing => im.`return`(0)
                    case Just(i) => for {
                        rest <- sum6
                        * <- im.`return` { i + rest }
                    } yield *
                }
            } yield *
        }

        def sum8: Iteratee[Int, Int] = Iteratee {
            import inner.forComp
            for {
                step <- runIteratee(sum6)
                * <- step match {
                    case Continue(k) => runIteratee { k { Chunks(List.range(1, 11)) } }
                    case _ => inner.`return`(step)
                }
            } yield *
        }
    }

    def testTrivial {
        val my = new My[IO.type]
        import IO.>>=
        val io = my.run_(my.sum8) >>= IO.print
        io.unIO()
    }
}
