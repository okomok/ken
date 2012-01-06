

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class EitherTest extends org.scalatest.junit.JUnit3Suite {

    def testEq {
        expect(true)(Eq.op_===(Left(10))(Left(10)))
        expect(false)(Eq.op_===(Left(10).of[Int, String])(Right[String]("abc").of[Int, String]))
        expect(true)(Eq.op_===(Right(10))(Right(10)))
    }

    def testOrd {
        val i = Ord[Either[Int, String]]
        import i._
        expect(false)(Left(3) > Right[String]("abc"))
        expect(true)(Left(3) < Right[String]("abc"))
        expect(true)(Left(3) < Left(10))
        expect(true)(Right[String]("abc") < Right[String]("abd"))
    }

    def testShow {
        val i = Show[Either[Int, String]]
        expect("Left(10)")(i.show(Left(10)).asJString)
        expect("Right(\"abcd\")")(i.show(Right[String]("abcd")).asJString)
    }
}
