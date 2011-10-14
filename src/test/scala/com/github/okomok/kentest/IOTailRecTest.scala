

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._ //{IO =>_, IORep => _, _}
import scala.annotation.tailrec


class IOTailRecTest extends org.scalatest.junit.JUnit3Suite {

/*
    type IORep[+a] = RealWorld.type => Product2[a, RealWorld.type]

    object Product2 {
        def unapply[x, y](p: Product2[x, y]): Option[(x, y)] = Some(p._1, p._2)
    }

    final case class IO[+a](override val old: IORep[a]) extends NewtypeOf[IORep[a]] {
        def ! : a = {
            @tailrec
            def loop(body: IO[a]): a = body match {
                case IO(rep) => rep(RealWorld) match {
                    case IOCall(rest) => loop(rest)
                    case IODone(result) => result
                }
            }
            loop(this)
        }
    }


    case class IOCall[+a](rest: IO[a]) extends Product2[a, RealWorld.type] {
        override def _1 = error("doh")//lazy val _1 = rest.!
        override val _2 = RealWorld
    }

    case class IODone[+a](result: a) extends Product2[a, RealWorld.type] {
        override val _1 = result
        override val _2 = RealWorld
    }

    object IO extends Monad[IO] with ThisIsInstance {
        // Overrides
        //
        private type m[+a] = IO[a]
        // Monad
        override def `return`[a](x: Lazy[a]): m[a] = returnIO(x)
        override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = bindIO(m)(k)

        private def returnIO[a](x: => a): IO[a] = IO { s => IODone(x) }

        private def bindIO[a, b](m: IO[a])(k: a => IO[b]): IO[b] = IO { s =>
            unIO(m)(RealWorld) match {
                case Product2(a, new_s) => unIO(k(a))(new_s)
            }
        }

        def unIO[a](io: IO[a]): IORep[a] = io.old

        def tailrec[a](io: => IO[a]): IO[a] = IO { s => IOCall(io) }

        val putStrLn: String => IO[Unit] = str => IO { s => IODone(Predef.println(List.toJString(str))) }
    }
*/
    // Overflow way
    //
    val isOdd1: Int => IO[Boolean] = {
        case 0 => for {
            _ <- IO.`return`()
            * <- IO.`return`(false)
        } yield *
        case n => for {
            _ <- IO.`return`()
            * <- isEven1(n - 1)
        } yield *
    }

    val isEven1: Int => IO[Boolean] = {
        case 0 => for {
            _ <- IO.`return`()
            * <- IO.`return`(true)
        } yield *
        case n => for {
            _ <- IO.`return`()
            * <- isOdd1(n - 1)
        } yield *
    }

    def teztTrivial {
        val io = isEven1(9)
        expect(false)(io.!)
    }

    def teztOverflow {
        val io = isEven1(9999)
        intercept[StackOverflowError] {
            io.!
        }
    }

    // TailRec way
    //   (for-expression is useless...)
    //

    val isOdd2: Int => IO[Boolean] = {
        case 0 => IO.`return`(false)
        case n => IO.`return`() >> IO.tailcall(isEven2(n - 1))
    }

    val isEven2: Int => IO[Boolean] = {
        case 0 => IO.`return`(true)
        case n => IO.`return`() >> IO.tailcall(isOdd2(n - 1))
    }

    def testNoOverflow2 {
        val io = isEven2(9999)
        expect(false)(io.!)
    }

    val isOdd3: Int => IO[Boolean] = {
        case 0 => for {
            _ <- IO.`return`()
        } IO.`return`(false)
        case n => for {
            _ <- IO.`return`()
        } IO.tailcall(isEven3(n - 1))
    }

    val isEven3: Int => IO[Boolean] = {
        case 0 => for {
            _ <- IO.`return`()
        } IO.`return`(true)
        case n => for {
            _ <- IO.`return`()
        } IO.tailcall(isOdd3(n - 1))
    }

    def testNoOverflow3 {
        val io = isEven3(9999)
        expect(false)(io.!)
    }
}
