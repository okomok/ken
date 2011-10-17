

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.quickchecktest


import com.github.okomok.ken._
import quickcheck._


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def prop_Int: Int => Bool = n => True

    def prop_Double: Double => Bool = n => True

    def prop_List[a]: List[a] => Bool = xs => {
        val l = List.length(xs)
        //println("length: " + l)
        l <= 40
    }

    def testTrivial {
        quickCheck(prop_Int).!
        quickCheck(prop_Double).!
        //Testable[Kind.const[List[Int] => Bool]].quickCheck(prop_reverse[Int]).!
        val tf = Testable[Kind.const[Bool]]
        //Testable.ofProperty.quickCheck(tf.forAll(Gen.choose(0, 10))(prop_int)).!

        //Testable.ofProperty.quickCheck(tf.forAllShrink(Gen.choose(0, 10))(Arbitary.ofInt.shrink)(prop_int)).!


        //Testable.ofProperty.quickCheck(tf.shrinking(Arbitary.ofInt.shrink)(50)(prop_int)).!

        //Testable[Kind.const[Double => Bool]].quickCheck(prop_Double).!
        // Testable[Kind.const[Char => Bool]].quickCheck(prop_char).!
    }

    def testInt {
        val tf = Testable[Kind.const[Int => Bool]]
        //tf.quickCheck(prop_Int).!
        //Testable.ofProperty.quickCheck(tf.forAll(Gen.choose(0, 10))(prop_Int)).!
        //Testable.ofProperty.quickCheck(tf.forAllShrink(Gen.choose(0, 10))(Arbitary.ofInt.shrink)(prop_Int)).!
    }

    def testList {
        quickCheck(prop_List[Int]).!

        val tf = Testable[Kind.const[List[Int] => Bool]]
        //tf.quickCheck(prop_List[Int]).! // ok.
    }
}
