

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ApplicativeTest extends org.scalatest.junit.JUnit3Suite {

    def testFunction1 {
        //import Function.toMonad, or
        val fm = Function.asMonad[Int]
        import fm._
        val r: Int => Int => Int = ((x: Int) => (y: Int) => x + y) <@> ((x: Int) => x)
    }

    def distList[f[+_], a](xs: List[f[a]])(implicit i: Applicative[f]): f[List[a]] = {
        import i._
        xs match {
            case Nil => pure(Nil.of[a])
            case x !:: xs => (List.op_!::[a] _) <@> x <*> distList(xs)
        }
    }

    def traverseList[f[+_], a, b](f: a => f[b])(xs: List[a])(implicit i: Applicative[f]): f[List[b]] = {
        import i._
        xs match {
            case Nil => pure(Nil.of[b])
            case x !:: xs => (List.op_!::[b] _) <@> f(x) <*> traverseList(f)(xs)
        }
    }

    trait MyTraversable[t[_]] {
        def traverse[f[+_], a, b](g: a => f[b])(x: t[a])(implicit i: Applicative[f]): f[t[b]]
        def dist[f[+_], a](x: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = traverse(id[f[a]])(x)
    }

    def accumulate[t[_], o, a](f: a => o)(x: t[a])(implicit i: MyTraversable[t], j: Monoid[o]): o = {
        import i._
        import j._
        // clearly, non-inference-able
        traverse[({type g[+x] = Const[o, x]})#g, a, a]((p: a) => Const[o, a](f(p)))(x).get
    }

    def miffy[m[+_], a](mb: m[Boolean])(mt: m[a])(me: m[a])(implicit i: Monad[m]): m[a] = {
        import i._
        for {
            b <- mb
            * <- if (b) mt else me // b can influence so that either mt or me is abandoned.
        } yield *
    }

    def iffy[f[+_], a](fb: f[Boolean])(ft: f[a])(fe: f[a])(implicit i: Applicative[f]): f[a] = {
        import i._
        def cond(b: Boolean)(t: a)(e: a): a = if (b) t else e // both ft and fe are computed.
        (cond _) <@> fb <*> ft <*> fe
    }
}
