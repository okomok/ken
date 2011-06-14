

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ListTest extends org.scalatest.junit.JUnit3Suite {

    def testZipList {
        import Applicative.{<@>, <*>}
        implicit val i = ZipList

        val xs = ((x: Int) => (y: Int) => (z: Int) => x + y + z) <@>
            List(1, 2, 3) <*> List(2, 3, 4) <*> List(3, 4, 5)

        expect(List(6, 9, 12))(xs)
    }

    def testRepeat {
        val xs = List.repeat(3)
        expect(List(3,3,3,3))(List.take(4)(xs))
    }

    def testCycle {
        val xs = List.cycle(List(1,2,3))
        expect(List(1,2,3,1,2,3,1,2,3,1))(List.take(10)(xs))
    }

    def testTranspose {
        import List.from
        val xs = List(List(1,2,3),List(4,5,6))
        val as = List(List(1,4),List(2,5),List(3,6))
        expect(as)(List.transpose(xs))
    }

    def testSubsequences {
        import List.from
        val xs = from("abc")
        val as = List(from(""),from("a"),from("b"),from("ab"),from("c"),from("ac"),from("bc"),from("abc"))
        expect(as)(List.subsequences(xs))
    }

    def testPermutations {
        import List.from
        val xs = from("abc")
        val as = List(from("abc"),from("bac"),from("cba"),from("bca"),from("cab"),from("acb"))
        expect(as)(List.permutations(xs))
    }

    def testLines {
        val xs = List.from("ABC\nDE\nFGHI\n")
        val as = List("ABC","DE","FGHI")
        expect(as)(List.map(List.stringize)(List.lines(xs)))
    }

    def testWords {
        val xs = List.from("ABC DE \n  FGHI   \n   ")
        val as = List("ABC","DE","FGHI")
        expect(as)(List.map(List.stringize)(List.words(xs)))
    }
}
