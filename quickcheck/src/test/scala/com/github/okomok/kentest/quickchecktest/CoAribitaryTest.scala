

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.quickchecktest


import com.github.okomok.ken._
import quickcheck._


class CoArbitaryTest extends org.scalatest.junit.JUnit3Suite {
    def test_ {}

    def teztList {
        val ia = CoArbitary[List[Int]]
        Gen.sample {
            ia.coarbitary(List(1,2,3,4))(Gen.choose(0, 50))
        }.!
    }
}
