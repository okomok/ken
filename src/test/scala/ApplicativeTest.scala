

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken
import ken.Prelude._
import ken.List


class ApplicativeTest extends org.scalatest.junit.JUnit3Suite {

    def testDummy {
    }

    def distList[f[_], a](xs: List[f[a]])(implicit i: ken.Applicative[f]): f[List[a]] = {
        import i._
        xs match {
            case List.Nil => pure(List.Nil)
            case List.::(x, xs) => List.cons[a] <#> x <*> distList(xs())
        }
    }

    def traverseList[f[_], a, b](f: a => f[b])(xs: List[a])(implicit i: ken.Applicative[f]): f[List[b]] = {
        import i._
        xs match {
            case List.Nil => pure(List.Nil)
            case List.::(x, xs) => List.cons[b] <#> f(x) <*> traverseList(f)(xs())
        }
    }

    trait Traversable[t[_]] {
        def traverse[f[_], a, b](g: a => f[b])(x: t[a])(implicit i: ken.Applicative[f]): f[t[b]]
        def dist[f[_], a](x: t[f[a]])(implicit i: ken.Applicative[f]): f[t[a]] = traverse(id[f[a]])(x)
    }

    def accumulate[t[_], o, a](f: a => o)(x: t[a])(implicit i: Traversable[t], j: ken.Monoid[o]): o = {
        import i._
        import j._
        // clealy, non-inference-able
        traverse[({type g[x] = ken.Const[o, x]})#g, a, a]((p: a) => ken.Const[o, a](f(p)))(x).getConst
    }

    def miffy[m[_], a](mb: m[Boolean])(mt: m[a])(me: m[a])(implicit i: ken.Monad[m]): m[a] = {
        import i._
        for {
            b <- mb
            r <- if (b) mt else me // b can influence so that either mt or me is abandoned.
        } yield r
    }

    def iffy[f[_], a](fb: f[Boolean])(ft: f[a])(fe: f[a])(implicit i: ken.Applicative[f]): f[a] = {
        import i._
        def cond(b: Boolean)(t: a)(e: a): a = if (b) t else e // both ft and fe are computed.
        (cond _) <#> fb <*> ft <*> fe
    }
}
