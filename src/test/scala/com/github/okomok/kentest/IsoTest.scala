

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class IsoTest extends org.scalatest.junit.JUnit3Suite {

    type x = WeakIdentity.apply[Int]
    val x: x = 3

/*
    implicit val ofWeakIdentity: Iso1[({type f[+a] = a})#f, ({type g[+a] = a})#g] = new Iso1[WeakIdentity.apply, ({type g[+a] = a})#g] {
        private type f[+a] = a
        private type g[+a] = a
        implicit def imply[a](f: f[a]): g[a] = f
        implicit def unimply[a](g: g[a]): f[a] = g
    }
*/

    trait AAA[x, +a]

    def testTrivial {
        // ken way is ok
        Iso1[List.type, List.type]
        Iso1[State.apply[Int], State.apply[Int]]
        Iso1[WeakIdentity.type, WeakIdentity.type]

        // primitive way
        implicitly[Iso1[WeakIdentity.apply, WeakIdentity.apply]] // ok; essentially same as ken way.
        /*
        implicitly[Iso1[({type m[+a] = AAA[Int, a]})#m, ({type m[+a] = AAA[Int, a]})#m]] // no without ofWeakIdentity
        implicitly[Iso1[({type m[+a] = a})#m, ({type m[+a] = a})#m]] // no without ofWeakIdentity
        implicitly[Iso1[WeakIdentity.apply, ({type m[+a] = a})#m]] // no without ofWeakIdentity
        */
    }

    class My[n[+_]] {
        def useIso1(implicit i: Iso1[n, WeakIdentity.apply]) = ()
//        def useIso2(implicit i: Iso1[n, ({type m[+a] = a})#m]) = ()
        def useIso3(implicit i: Iso1[n, Identity]) = ()
    }

    def testWeak {
        val my = new My[({type m[+a] = a})#m]
        // my.useIso1 // no without ofWeakIdentity
        //my.useIso2
    }

    def testWeak_ {
        val my = new My[WeakIdentity.apply]
        my.useIso1
        //my.useIso2
    }

    def testStrong {
        val my = new My[Identity]
        my.useIso3
    }


    class My2[n <: Kind.Function1] {
        type m[+a] = n#apply[a]
        def useIso1(implicit i: Iso1[m, WeakIdentity.apply]) = ()
        def useIso3(implicit i: Iso1[m, Identity]) = ()
    }

    def testWeak2 {
        val my = new My2[WeakIdentity.type]
        my.useIso1
    }

    def testStrong2 {
        val my = new My[Identity]
        my.useIso3
    }

    def testParsecPrim {
        implicitly[parsec.Stream[List[Int], WeakIdentity.apply, Int]]
    }

}
