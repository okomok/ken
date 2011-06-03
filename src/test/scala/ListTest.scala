

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ListTest extends org.scalatest.junit.JUnit3Suite {

    def testZipList {
        import Applicative._
        implicit val i = ZipList

        val xs = ((x: Int) => (y: Int) => (z: Int) => x + y + z) <#>
            List(1, 2, 3) <*> List(2, 3, 4) <*> List(3, 4, 5)

        expect(List(6, 9, 12))(xs)
    }
}
