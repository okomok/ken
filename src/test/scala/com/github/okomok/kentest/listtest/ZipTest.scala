

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest
package listtest


import com.github.okomok.ken._


class ZipTest extends org.scalatest.junit.JUnit3Suite {
    def testFibs: Unit = {
        lazy val fibs: List[Int] = 0 :: 1 :: List.map[(Int, Int), Int](xy => xy._1 + xy._2)(List.zip(fibs)(List.tail(fibs)))
        expect(832040)(fibs.!!(30))
    }

    def testUnzipInfinite: Unit = {
        val L = List.unzip { List.cycle(('a', 1) !:: ('b', 2) !:: ('c', 3) !:: Nil) }
        val A1 = 'a' !:: 'b' !:: 'c' !:: 'a' !:: 'b' !:: Nil
        val A2 = 1 !:: 2 !:: 3 !:: 1 !:: 2 !:: 3 !:: 1 !:: Nil
        expect(A1)(List.take(5)(L._1))
        expect(A2)(List.take(7)(L._2))
    }
}
