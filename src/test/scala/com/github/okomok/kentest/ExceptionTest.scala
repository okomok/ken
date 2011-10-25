

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ExceptionTest extends org.scalatest.junit.JUnit3Suite {

    sealed abstract class MyException
    case object ThisException extends MyException
    case object ThatException extends MyException

    object MyException extends Exception[MyException] with Eq.Of[MyException] with Show.Of[MyException] with
        ThisIsInstance
    {
        override val typeOf: typeOf = _ => implicitly[ClassManifest[MyException]]
    }

    def testTrivial {
        val i = Exception[MyException.type]

        val x = i.toException(ThisException)
        val t = i.fromException(x)
        expect(Just(ThisException))(t)
    }

    def testMismatch {
        val i = Exception[MyException.type]
        val s = SomeException(ErrorCall("hello"))
        val t = i.fromException(s)
        expect(Nothing)(t)
    }
}



class ImplicitLocalTezt {

/*
    trait Foo[x]

    class TestOk {
        trait My
        object My {
            implicit def Foo[My]: Foo[My] = new Foo[My] {}
        }

        implicitly[Foo[My]]
    }

    def testNo {
        trait My
        object My {
            implicit def Foo[My]: Foo[My] = new Foo[My] {}
        }

        implicitly[Foo[My]]
    }

*/
}
