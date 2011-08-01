

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


// See: http://d.hatena.ne.jp/akihiro4chawon/20110424/1303626015


class HammingTest extends org.scalatest.junit.JUnit3Suite {
    def mult(x: Int)(y: BigInt): BigInt = y * x
    val hamming: List[BigInt] = BigInt(1) :: merge(merge(List.map(mult(2))(hamming))(List.map(mult(3))(hamming)))(List.map(mult(5))(hamming))

    def merge[a](xs_ : List[a])(ys_ : List[a])(implicit i: Ord[a]): List[a] = {
        import i._
        val x :: xs = xs_
        val y :: ys = ys_
        if (x < y) {
            x :: merge(xs.!)(ys_)
        } else if (x > y) {
            y :: merge(xs_)(ys.!)
        } else {
            x :: merge(xs.!)(ys.!)
        }
    }

    def testTrivial {
        expect(BigInt("2125764000"))(hamming !! 1690)
        // expect(BigInt("519312780448388736089589843750000000000000000000000000000000000000000000000000000000"))(hamming !! 1000000-1)
    }
}
