

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    def ! : a
}


object Lazy extends Monad[Lazy] with ThisIsInstance {
    implicit def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val ! : a = x
    }

    implicit def eval[a](x: Lazy[a]): a = x.!

    // Overrides
    //
    // Monad
    private[this] type m[+a] = Lazy[a]
    override def `return`[a](x: Lazy[a]): m[a] = x
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = Lazy(y(x.!).!)

    // Implicits for functions
    //
    implicit def toStrict1[a, b](f: Lazy[a] => b): a => b = x => f(x)
    implicit def toStrict2[a, b, c](f: a => Lazy[b] => c): a => b => c = x => y => f(x)(y.!)

    implicit def toLazy1[a, b](f: a => b): Lazy[a] => b = x => f(x)
    implicit def toLazy2[a, b, c](f: a => b => c): a => Lazy[b] => c = x => y => f(x)(y.!)

    implicit def toLazyResult1[a, b](f: a => b): a => Lazy[b] = x => Lazy(f(x))
    implicit def toLazyResult2[a, b, c](f: a => b => c): a => b => Lazy[c] = x => y => Lazy(f(x)(y))

    implicit def toStrictResult1[a, b](f: a => Lazy[b]): a => b = x => f(x).!
    implicit def toStrictResult2[a, b, c](f: a => b => Lazy[c]): a => b => c = x => y => f(x)(y).!
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x.!)
}
