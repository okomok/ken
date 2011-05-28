

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken
package detail


object Test {

    val x: List[Int] = locally {
        import Applicative.List._
        ((x: Int) => (y: Int) => x + y) <#> (2 :: 3 :: 4 :: Nil) <*> pure(4)
    }

    def callfmap[f[_], a, b](x: Functor[f])(y: a => b)(z: f[a]): f[b] = x.fmap(y)(z)

    callfmap(ListInstance)((x: Int) => x)(3 :: Nil)

}
