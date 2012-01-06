// Public domain

package com.github.okomok.kentest.example.vshaskell

import com.github.okomok.ken._

class LiftingTest extends org.scalatest.junit.JUnit3Suite {

    def testLiftM {
/*
        a = liftM (* 3) [1,2,3]
*/
        // ken tends to prefer "do-Notation" for type-inference issue.
        // Ditto `fmap`.
        val a = for { x <- List(1,2,3) } yield x * 3

        expect(List(3,6,9))(a)
    }

}

// References
//
// http://ujihisa.blogspot.com/2010/08/haskell-liftm-practice.html

