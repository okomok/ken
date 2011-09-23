

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait ArrowLoop[a[-_, +_]] extends Arrow[a] {
    final val asArrowLoop: ArrowLoop[apply2] = this

    // Core
    //
    def loop[b, c, d](f: a[(b, Lazy[d]), (Lazy[c], Lazy[d])]): a[b, c]
}


trait ArrowLoopProxy[a[-_, +_]] extends ArrowLoop[a] with ArrowProxy[a] {
    def selfArrowLoop: ArrowLoop[a]
    override def selfArrow: Arrow[a] = selfArrowLoop

    override def loop[b, c, d](f: a[(b, Lazy[d]), (Lazy[c], Lazy[d])]): a[b, c] = selfArrowLoop.loop(f)
}


object ArrowLoop {
    def apply[a <: Kind.Function2](implicit i: ArrowLoop[a#apply2]): ArrowLoop[a#apply2] = i

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: ArrowLoop[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): ArrowLoop[nt#apply2] = new ArrowLoop[nt#apply2] with ArrowProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfArrow = Arrow.deriving[nt, ot]

        override def loop[b, c, d](f: a[(b, Lazy[d]), (Lazy[c], Lazy[d])]): a[b, c] = j.newOf(i.loop(j.oldOf(f)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowLoop[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowLoop[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}
