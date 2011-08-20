

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Arrow[a[-_, +_]] extends Category[a] {
    final val asArrow: Arrow[apply2] = this

    // Core
    //
    def arr[b, c](f: b => c): a[b, c]
    def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)]
    def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = {
        def swap[x, y](v: (x, y)): (y, x) = (v._2, v._1)
        arr(swap[d, b]) >>> first(f) >>> arr(swap[c, d])
    }

    // Overrides
    //
    // Category
    override def cid[a_]: a[a_, a_] = arr(id[a_])

    // Extra
    //
    def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = first(f) >>> second(g)
    def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = arr((b: b) => (b, b)) >>> f *** g

    def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = arr(f) >>> a
    def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = a >>> arr(f)
    def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = a <<< arr(f)
    def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = arr(f) <<< a

    def returnA[b]: a[b, b] = arr(id[b])

    // Operators
    //
    sealed class Op_***[b, c](f: a[b, c]) {
        def ***[b_, c_](g: a[b_, c_]): a[(b, b_), (c, c_)] = op_***(f)(g)
    }
    final implicit def ***[b, c](f: a[b, c]): Op_***[b, c] = new Op_***[b, c](f)

    sealed class Op_&&&[b, c](f: a[b, c]) {
        def &&&[c_](g: a[b, c_]): a[b, (c, c_)] = op_&&&(f)(g)
    }
    final implicit def &&&[b, c](f: a[b, c]): Op_&&&[b, c] = new Op_&&&[b, c](f)

    sealed class Op_^>>[b, c](f: b => c) {
        def ^>>[d](a: a[c, d]): a[b, d] = op_^>>(f)(a)
    }
    final implicit def ^>>[b, c](f: b => c): Op_^>>[b, c] = new Op_^>>[b, c](f)

    sealed class Op_>>^[b, c](a: a[b, c]) {
        def >>^[d](f: c => d): a[b, d] = op_>>^(a)(f)
    }
    final implicit def >>^[b, c](a: a[b, c]): Op_>>^[b, c] = new Op_>>^[b, c](a)

    sealed class Op_<<^[c, d](a: a[c, d]) {
        def <<^[b](f: b => c): a[b, d] = op_<<^(a)(f)
    }
    final implicit def <<^[c, d](a: a[c, d]): Op_<<^[c, d] = new Op_<<^[c, d](a)

    sealed class Op_^<<[c, d](f: c => d) {
        def ^<<[b](a: a[b, c]): a[b, d] = op_^<<(f)(a)
    }
    final implicit def ^<<[c, d](f: c => d): Op_^<<[c, d] = new Op_^<<[c, d](f)
}


trait ArrowProxy[a[-_, +_]] extends Arrow[a] with CategoryProxy[a] {
    def selfArrow: Arrow[a]
    override def selfCategory: Category[a] = selfArrow

    override def arr[b, c](f: b => c): a[b, c] = selfArrow.arr(f)
    override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = selfArrow.first(f)
    override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = selfArrow.second(f)

    override def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = selfArrow.op_***(f)(g)
    override def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = selfArrow.op_&&&(f)(g)
    override def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = selfArrow.op_^>>(f)(a)
    override def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = selfArrow.op_>>^(a)(f)
    override def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = selfArrow.op_<<^(a)(f)
    override def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = selfArrow.op_^<<(f)(a)
    override def returnA[b]: a[b, b] = selfArrow.returnA[b]
}


object Arrow {
    def apply[a <: Kind.Function2](implicit i: Arrow[a#apply2]): Arrow[a#apply2] = i

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: Arrow[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): Arrow[nt#apply2] = new Arrow[nt#apply2] with CategoryProxy[nt#apply2] {
        private[this] type a[-a, +b] = nt#apply2[a, b]
        override val selfCategory = Category.deriving[nt, ot](i, j)

        override def arr[b, c](f: b => c): a[b, c] = j.newOf(i.arr(f))
        override def first[b, c, d](f: a[b, c]): a[(b, d), (c, d)] = j.newOf(Lazy(i.first(j.oldOf(Lazy(f)))))
        override def second[b, c, d](f: a[b, c]): a[(d, b), (d, c)] = j.newOf(Lazy(i.second(j.oldOf(Lazy(f)))))

        override def op_***[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = j.newOf(i.op_***(j.oldOf(f))(j.oldOf(g)))
        override def op_&&&[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = j.newOf(i.op_&&&(j.oldOf(f))(j.oldOf(g)))
        override def op_^>>[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = j.newOf(i.op_^>>(f)(j.oldOf(a)))
        override def op_>>^[b, c, d](a: a[b, c])(f: c => d): a[b, d] = j.newOf(i.op_>>^(j.oldOf(a))(f))
        override def op_<<^[b, c, d](a: a[c, d])(f: b => c): a[b, d] = j.newOf(i.op_<<^(j.oldOf(a))(f))
        override def op_^<<[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = j.newOf(i.op_^<<(f)(j.oldOf(a)))
        override def returnA[b]: a[b, b] = j.newOf(i.returnA[b])
    }

    def weak[nt <: Kind.Newtype2](implicit i: Arrow[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): Arrow[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}
