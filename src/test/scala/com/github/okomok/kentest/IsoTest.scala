

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class IsoTest extends org.scalatest.junit.JUnit3Suite {

    class My[n[+_]] {
        def useIso1(implicit i: Iso[n, WeakIdentity.apply]) = ()
        def useIso2(implicit i: Iso[n, ({type m[+a] = a})#m]) = ()
        def useIso3(implicit i: Iso[n, Identity]) = ()
    }

    def testWeak {
        val my = new My[({type m[+a] = a})#m]
        my.useIso1
        my.useIso2
    }

    def testWeak_ {
        val my = new My[WeakIdentity.apply]
        my.useIso1
        my.useIso2
    }

    def testStrong {
        val my = new My[Identity]
        my.useIso3
    }

}
