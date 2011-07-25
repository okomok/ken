

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class WeakTest extends org.scalatest.junit.JUnit3Suite {

    val w = Identity.weak
    import w._

    def testImplicit {
        implicitly[Functor[({type m[+a] = a})#m]]
        implicitly[Applicative[({type m[+a] = a})#m]]
        implicitly[Monad[({type m[+a] = a})#m]]
    }
}
