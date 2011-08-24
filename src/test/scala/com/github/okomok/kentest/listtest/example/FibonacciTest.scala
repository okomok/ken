

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest; package example


    import com.github.okomok.ken._

    class FibonacciTest extends org.scalatest.junit.JUnit3Suite {
        def testTrivial {
            lazy val fibs: List[Int] = 0 :: 1 :: List.map[(Int, Int), Int](xy => xy._1 + xy._2)(List.zip(fibs)(List.tail(fibs)))
            expect(832040)(fibs.!!(30))
        }
    }
