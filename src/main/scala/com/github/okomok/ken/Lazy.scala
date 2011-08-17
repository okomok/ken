

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


sealed abstract class Lazy[+a] {
    private[ken] def _eval: a
}


object Lazy extends Monad[Lazy] with ThisIsInstance with LazyEval {
    implicit def apply[a](x: => a): Lazy[a] = new Lazy[a] {
        override lazy val _eval : a = x
    }

    implicit def eval[a](x: Lazy[a]): a = x._eval

    // Overrides
    //
    // Monad
    private[this] type m[+a] = Lazy[a]
    override def `return`[a](x: Lazy[a]): m[a] = x
    override def op_>>=[a, b](x: m[a])(y: a => m[b]): m[b] = Lazy(y(x._eval)._eval)

    // Implicits for functions
    //
    implicit def toStrict1[a, b](f: Lazy[a] => b): a => b = x => f(Lazy(x))
    implicit def toStrict2[a, b, c](f: a => Lazy[b] => c): a => b => c = x => y => f(x)(Lazy(y))

    implicit def toLazy1[a, b](f: a => b): Lazy[a] => b = x => f(x._eval)
    implicit def toLazy2[a, b, c](f: a => b => c): a => Lazy[b] => c = x => y => f(x)(y._eval)

    implicit def toLazyResult1[a, b](f: a => b): a => Lazy[b] = x => Lazy(f(x))
    implicit def toLazyResult2[a, b, c](f: a => b => c): a => b => Lazy[c] = x => y => Lazy(f(x)(y))

    implicit def toStrictResult1[a, b](f: a => Lazy[b]): a => b = x => f(x)._eval
    implicit def toStrictResult2[a, b, c](f: a => b => Lazy[c]): a => b => c = x => y => f(x)(y)._eval
}


// Lower priority
private[ken] trait LazyEval { this: Lazy.type =>
    sealed class _Postfix_![a](x: Lazy[a]) {
        def ! : a = x._eval
    }
    implicit def _postfix_![a](x: Lazy[a]): _Postfix_![a] = new _Postfix_!(x)
}


object ! {
    def unapply[a](x: Lazy[a]): Option[a] = Some(x._eval)
}
