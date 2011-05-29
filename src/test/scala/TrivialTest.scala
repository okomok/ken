

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken


class TrivialTest extends org.scalatest.junit.JUnit3Suite {

    def testTrivial {
        val x: List[Int] = locally {
            import ken.Applicative.List._
            ((x: Int) => (y: Int) => x + y) <#> (2 :: 3 :: 4 :: Nil) <*> pure(4)
        }

        def callfmap[f[_], a, b](x: ken.Functor[f])(y: a => b)(z: f[a]): f[b] = x.fmap(y)(z)

        callfmap(ken.Applicative.List)((x: Int) => x)(3 :: Nil)
    }

}
