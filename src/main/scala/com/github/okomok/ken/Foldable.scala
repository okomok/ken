

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Foldable[t[+_]] extends Klass {
    type apply[+a] = t[a]
    final def asFoldable: Foldable[t] = this

// Overridables
    def fold[m](f: t[m])(implicit i: Monoid[m]): m = foldMap(id[m])(f)
    def foldMap[a, m](f: a => m)(x: t[a])(implicit i: Monoid[m]): m = foldr[a, m](i.mappend compose f)(i.mempty)(x)

    def foldr[a, b](f: a => (=> b) => b)(z: b)(t: t[a]): b = foldMap((a: a) => (b: b) => f(a)(b))(t)(Endo.weak[b].monoid)(z)
    def foldl[a, b](f: a => b => a)(z: a)(t: t[b]): a = foldMap(flip(f))(t)(Endo.weak[a].monoid.dual)(z)

    def foldr1[a](f: a => (=> a) => a)(xs: t[a]): a = {
        def mf(x: a)(y: => Maybe[a]): Maybe[a] = y match {
            case Nothing => Just(x)
            case Just(y) => Just(f(x)(y))
        }
        Maybe.fromMaybe(error("foldr1: empty structure"))(foldr(mf)(Nothing)(xs))
    }

    def foldl1[a](f: a => a => a)(xs: t[a]):a = {
        val mf: Maybe[a] => a => Maybe[a] = {
            case Nothing => y => Just(y)
            case Just(x) => y => Just(f(x)(y))
        }
        Maybe.fromMaybe(error("foldl1: empty structure"))(foldl(mf)(Nothing)(xs))
    }

// Utilities
}

/*
trait FoldableProxy[t[+_]] extends Foldable[t] with Proxy {
    override def self: Foldable[f]
    override def fmap[a, b](x: a => b)(y: f[a]): f[b] = self.fmap(x)(y)
}
*/

object Foldable {
    def apply[t[+_]](implicit i: Foldable[t]): Foldable[t] = i
}
