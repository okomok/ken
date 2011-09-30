

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.quickchecktest


import com.github.okomok.ken._
import quickcheck._


class ArbitaryTest extends org.scalatest.junit.JUnit3Suite {
    def test_ {}

    def teztInt {
        val ia = Arbitary[Int.type]
        Gen.sample(ia.arbitary).!
        println( ia.shrink(10) )
    }

    def teztDouble {
        val ia = Arbitary[Double.type]
        Gen.sample(ia.arbitary).!
        println( ia.shrink(14315.1431D) )
    }

    def teztRatio {
        val ia = Arbitary[Ratio[Int]]
        Gen.sample(ia.arbitary).!
        println( ia.shrink(Ratio(12,16)) )
    }

    def teztFunction {
        val ia = Arbitary[Kind.const[Int => Int]]
    }

    def teztList {
        val ia = Arbitary[List[Int]]
        Gen.sample(ia.arbitary).!
        println( ia.shrink(List(12,16)) )
    }
}
