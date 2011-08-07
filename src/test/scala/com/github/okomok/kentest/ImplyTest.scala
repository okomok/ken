

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ImplyTest extends org.scalatest.junit.JUnit3Suite {

    def testImplicit {
        val w = Identity.weak
        import w._

        implicitly[Functor[({type m[+a] = a})#m]]
        implicitly[Applicative[({type m[+a] = a})#m]]
        implicitly[Monad[({type m[+a] = a})#m]]

        implicitly[Monad[Identity.asMonad.apply]]
    }

    def testNoAmbiguity {
        final class ParseError(val location: Int, val reason: String_)

        object Err {
            def apply(l: Int)(r: String_): ParseError = new ParseError(l, r)
        }

        implicit object ParseErrorClass extends ErrorClass[ParseError] {
            override def noMsg = Err(0)("ParseError")
            override def strMsg = s => Err(0)(s)
        }

        val w = Error.weak[ParseError]
        import w._
        implicitly[Monad[({type m[+a] = Either[ParseError, a]})#m]]
    }

    def testCreateInstance {
        val wi = Weak1[Identity.type]
        val ei = Weak0[Endo.apply[Int]]
    }
}
