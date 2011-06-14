

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._


class ApplicativeTest extends org.scalatest.junit.JUnit3Suite {

    def testDummy {
    }

    def distList[f[_], a](xs: List[f[a]])(implicit i: Applicative[f]): f[List[a]] = {
        import Applicative._
        xs match {
            case Nil => pure(Nil.of[a])(i)
            case x !:: xs => (List.op_!::[a] _) <@> x <*> distList(xs)
        }
    }

    def traverseList[f[_], a, b](f: a => f[b])(xs: List[a])(implicit i: Applicative[f]): f[List[b]] = {
        import Applicative._
        xs match {
            case Nil => pure(Nil.of[b])(i)
            case x !:: xs => (List.op_!::[b] _) <@> f(x) <*> traverseList(f)(xs)
        }
    }

    trait MyTraversable[t[_]] {
        def traverse[f[_], a, b](g: a => f[b])(x: t[a])(implicit i: Applicative[f]): f[t[b]]
        def dist[f[_], a](x: t[f[a]])(implicit i: Applicative[f]): f[t[a]] = traverse(id[f[a]])(x)
    }

    def accumulate[t[_], o, a](f: a => o)(x: t[a])(implicit i: MyTraversable[t], j: Monoid[o]): o = {
        import i._
        import j._
        // clealy, non-inference-able
        traverse[({type g[x] = Const[o, x]})#g, a, a]((p: a) => Const[o, a](f(p)))(x).getConst
    }

    def miffy[m[_], a](mb: m[Boolean])(mt: m[a])(me: m[a])(implicit i: Monad[m]): m[a] = {
        import Monad._
        for {
            b <- mb
            r <- if (b) mt else me // b can influence so that either mt or me is abandoned.
        } yield r
    }

    def iffy[f[_], a](fb: f[Boolean])(ft: f[a])(fe: f[a])(implicit i: Applicative[f]): f[a] = {
        import Applicative._
        def cond(b: Boolean)(t: a)(e: a): a = if (b) t else e // both ft and fe are computed.
        (cond _) <@> fb <*> ft <*> fe
    }
}
