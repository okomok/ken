

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class FunctorTest extends org.scalatest.junit.JUnit3Suite {

    def testFor {
        val f = Functor[Const.apply[Int]]
        import f.`for`

        val g: Const[Int, Char] = for {
            d <- Const[Int, Double](99)
        } yield 'a'
    }

}
