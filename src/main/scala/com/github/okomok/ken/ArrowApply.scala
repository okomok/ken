

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

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: ArrowApply[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): ArrowApply[nt#apply2] = new ArrowApply[nt#apply2] with ArrowProxy[nt#apply2] {
        private[this] type a[-a, +b] = nt#apply2[a, b]
        override val self = Arrow.deriving[nt, ot](i, j)

        override def app[b, c]: a[(a[b, c], b), c] = j.new2(i.op_^>>( (n: (nt#apply2[b, c], b)) => (j.old2(n._1), n._2) )(i.app[b, c]))

        override def leftApp[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = j.new2(i.leftApp(j.old2(f)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowApply[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowApply[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}
