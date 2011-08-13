

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait ArrowApply[a[-_, +_]] extends Arrow[a] {
    final val asArrowApply: ArrowApply[apply2] = this

    // Core
    //
    def app[b, c]: a[(a[b, c], b), c]

    // Extra
    //
    def leftApp[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = {
        val j = ArrowChoice[Function.type]
        import j.|||
        arr {
            ((b: b) => (arr((_: Unit) => b) >>> f >>> arr(Left(_).of[c, d]), ())) |||
            ((d: d) => (arr((_: Unit) => d) >>> arr(Right(_).of[c, d]), ()))
        } >>> app
    }

    // Monads
    //
    final lazy val _arrowMonads = new _ArrowMonads[a](this)
    type ArrowMonad[+b] = _arrowMonads._ArrowMonad[b]
    final lazy val ArrowMonad = _arrowMonads._ArrowMonad
}


trait ArrowApplyProxy[a[-_, +_]] extends ArrowApply[a] with ArrowProxy[a] {
    override def self: ArrowApply[a]

    override def app[b, c]: a[(a[b, c], b), c] = self.app[b, c]

    override def leftApp[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = self.leftApp(f)
}


object ArrowApply {
    def apply[a <: Kind.Function2](implicit i: ArrowApply[a#apply2]): ArrowApply[a#apply2] = i
}
