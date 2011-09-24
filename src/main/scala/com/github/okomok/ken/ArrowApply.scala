

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ArrowApply[a[-_, +_]] extends Arrow[a] {
    final val asArrowApply: ArrowApply[apply2] = this

    // Core
    //
    def app[b, c]: a[(a[b, c], b), c]

    // Extra
    //
    def leftApp[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = {
        val j = ArrowChoice[Function.type]
        import j.|||:
        arr {
            ((b: b) => (arr((_: Unit) => b) >>>: f >>>: arr(Left(_: c).of[c, d]), ())) |||:
            ((d: d) => (arr((_: Unit) => d) >>>: arr(Right(_: d).of[c, d]), ()))
        } >>>: app
    }

    // Monads
    //
    final lazy val _arrowMonads = new _ArrowMonads[a](this)
    type ArrowMonad[+b] = _arrowMonads._ArrowMonad[b]
    final lazy val ArrowMonad = _arrowMonads._ArrowMonad
}


trait ArrowApplyProxy[a[-_, +_]] extends ArrowApply[a] with ArrowProxy[a] {
    def selfArrowApply: ArrowApply[a]
    override def selfArrow: Arrow[a] = selfArrowApply

    override def app[b, c]: a[(a[b, c], b), c] = selfArrowApply.app[b, c]

    override def leftApp[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = selfArrowApply.leftApp(f)
}


object ArrowApply {
    def apply[a <: Kind.Function2](implicit i: ArrowApply[a#apply2]): ArrowApply[a#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit i: ArrowApply[nt#oldtype2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowApply[nt#apply2] = new ArrowApply[nt#apply2] with ArrowProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfArrow = Arrow.deriving[nt]

        override def app[b, c]: a[(a[b, c], b), c] = j.newOf(i.op_^>>:( (n: (nt#apply2[b, c], b)) => (j.oldOf(n._1), n._2) )(i.app[b, c]))

        override def leftApp[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = j.newOf(Lazy(i.leftApp(j.oldOf(Lazy(f)))))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowApply[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowApply[nt#oldtype2] = deriving[Kind.dualNewtype2[nt]](i, j.dual)
}
