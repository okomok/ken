

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait MonadPlus[m[+_]] extends Monad[m] with Alternative[m] { outer =>
    def mzero: m[Nothing]
    def mplus[a](x: m[a])(y: => m[a]): m[a]

    override def empty: m[Nothing] = mzero
    override def op_<|>[a](x: m[a])(y: => m[a]): m[a] = mplus(x)(y)

    override implicit def method[a](x: m[a]): MonadPlusMethod[m, a] = new MonadPlusMethod[m, a] {
        override def klass = outer
        override def callee = x
    }

    final def guard(b: Bool): m[Unit] = b match {
        case true => `return`()
        case false => mzero
    }

    final def msum[a](xs: List[m[a]]): m[a] = List.foldr(mplus[a])(mzero)(xs)
}


trait MonadPlusMethod[m[+_], +a] extends MonadMethod[m, a] with AlternativeMethod[m, a] {
    override def klass: MonadPlus[m]
    final def _mplus_[b >: a](y: => m[b]): m[b] = klass.mplus[b](callee)(y)
}


trait MonadPlusProxy[m[+_]] extends MonadPlus[m] with MonadProxy[m] with AlternativeProxy[m] {
    override def self: MonadPlus[m]
    override def mzero: m[Nothing] = self.mzero
    override def mplus[a](x: m[a])(y: => m[a]): m[a] = self.mplus(x)(y)
}


object MonadPlus {
    def apply[m[+_]](implicit i: MonadPlus[m]): MonadPlus[m] = i
}
