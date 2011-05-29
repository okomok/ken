

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val xs: List[Int] = locally {
            import ken.List_._
            ((x: Int) => (y: Int) => x + y) <#> (2 :: 3 :: 4 :: Nil) <*> pure(4)
        }
        expect(6 :: 7 :: 8 :: Nil)(xs)

        /*
        def callfmap[x <: ken.Functor, a, b](x: x)(y: a => b)(z: f[a]): f[b] = x.fmap(y)(z)

        callfmap(ken.List_)((x: Int) => x)(3 :: Nil)
        */
    }

}
