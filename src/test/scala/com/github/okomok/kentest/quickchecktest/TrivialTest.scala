

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.quickchecktest


import com.github.okomok.ken._
import quickcheck._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def prop_Int: Int => Bool = n => {
        println("sample: " + n)
        0 <= n && n <= 9
    }

    def prop_Double: Double => Bool = n => {
        println("sample: " + n)
        True
    }

    def prop_List[a](xs: List[a]): Bool = {
        val l = List.length(xs)
        println("length: " + l)
        l <= 80
    }

    def testTrivial {
        //Testable[Kind.const[List[Int] => Bool]].quickCheck(prop_reverse[Int]).!
        val tf = Testable[Kind.const[Bool]]
        //Testable.ofProperty.quickCheck(tf.forAll(Gen.choose(0, 10))(prop_int)).!

        //Testable.ofProperty.quickCheck(tf.forAllShrink(Gen.choose(0, 10))(Arbitary.ofInt.shrink)(prop_int)).!


        //Testable.ofProperty.quickCheck(tf.shrinking(Arbitary.ofInt.shrink)(50)(prop_int)).!

        //Testable[Kind.const[Double => Bool]].quickCheck(prop_double).! // hungup!
       // Testable[Kind.const[Char => Bool]].quickCheck(prop_char).!
   }

   def testInt {
        val tf = Testable[Bool.type]
        //Testable.ofProperty.quickCheck(tf.forAll(Gen.choose(0, 10))(prop_Int)).!
        //Testable.ofProperty.quickCheck(tf.forAllShrink(Gen.choose(0, 10))(Arbitary.ofInt.shrink)(prop_Int)).!
   }

   def testList {
        val tf = Testable[Kind.const[List[Int] => Bool]]
        //tf.quickCheck(prop_List[Int]).! // stack-overflow!
   }
}
