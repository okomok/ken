

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


// @pending("not work well under the scalac inferencer")
trait Arrow[a[-_, +_]] extends Category[a] {
    final val asArrow: Arrow[apply2] = this

    // Core
    //
    def arr[b, c](f: b => c): a[b, c]
    def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)]
    def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = {
        def swap[x, y](v: (x, y)): (y, x) = (v._2, v._1)
        arr(swap[d, b]) >>>: first(f, Type[d]) >>>: arr(swap[c, d])
    }

    // Overrides
    //
    // Category
    override def cid[a_]: a[a_, a_] = arr(id[a_])

    // Extra
    //
    def op_***:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = first(f, Type[b_]) >>>: second(g)
    def op_&&&:[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = arr((b: b) => (b, b)) >>>: f ***: g

    def op_^>>:[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = arr(f) >>>: a
    def op_>>^:[b, c, d](a: a[b, c])(f: c => d): a[b, d] = a >>>: arr(f)
    def op_<<^:[b, c, d](a: a[c, d])(f: b => c): a[b, d] = a <<<: arr(f)
    def op_^<<:[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = arr(f) <<<: a

    def returnA[b]: a[b, b] = arr(id[b])

    // Operators
    //
    private[ken] sealed class Op_***:[b_, c_](g: a[b_, c_]) {
        def ***:[b, c](f: a[b, c]): a[(b, b_), (c, c_)] = op_***:(f)(g)
    }
    final implicit def ***:[b_, c_](g: a[b_, c_]): Op_***:[b_, c_] = new Op_***:(g)

    private[ken] sealed class Op_&&&:[b, c_](g: a[b, c_]) {
        def &&&:[c](f: a[b, c]): a[b, (c, c_)] = op_&&&:(f)(g)
    }
    final implicit def &&&:[b, c_](g: a[b, c_]): Op_&&&:[b, c_] = new Op_&&&:(g)

    private[ken] sealed class Op_^>>:[c, d](a: a[c, d]) {
        def ^>>:[b](f: b => c): a[b, d] = op_^>>:(f)(a)
    }
    final implicit def ^>>:[c, d](a: a[c, d]): Op_^>>:[c, d] = new Op_^>>:(a)

    private[ken] sealed class Op_>>^:[c, d](f: c => d) {
        def >>^:[b](a: a[b, c]): a[b, d] = op_>>^:(a)(f)
    }
    final implicit def >>^:[c, d](f: c => d): Op_>>^:[c, d] = new Op_>>^:(f)

    private[ken] sealed class Op_<<^:[b, c](f: b => c) {
        def <<^:[d](a: a[c, d]): a[b, d] = op_<<^:(a)(f)
    }
    final implicit def <<^:[b, c](f: b => c): Op_<<^:[b, c] = new Op_<<^:(f)

    private[ken] sealed class Op_^<<:[b, c](a: a[b, c]) {
        def ^<<:[d](f: c => d): a[b, d] = op_^<<:(f)(a)
    }
    final implicit def ^<<:[b, c](a: a[b, c]): Op_^<<:[b, c] = new Op_^<<:(a)
}


trait ArrowProxy[a[-_, +_]] extends Arrow[a] with CategoryProxy[a] {
    type selfArrow = Arrow[a]
    def selfArrow: selfArrow
    override def selfCategory: selfCategory = selfArrow

    override def arr[b, c](f: b => c): a[b, c] = selfArrow.arr(f)
    override def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)] = selfArrow.first(f)
    override def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = selfArrow.second(f)

    override def op_***:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = selfArrow.op_***:(f)(g)
    override def op_&&&:[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = selfArrow.op_&&&:(f)(g)
    override def op_^>>:[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = selfArrow.op_^>>:(f)(a)
    override def op_>>^:[b, c, d](a: a[b, c])(f: c => d): a[b, d] = selfArrow.op_>>^:(a)(f)
    override def op_<<^:[b, c, d](a: a[c, d])(f: b => c): a[b, d] = selfArrow.op_<<^:(a)(f)
    override def op_^<<:[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = selfArrow.op_^<<:(f)(a)
    override def returnA[b]: a[b, b] = selfArrow.returnA[b]
}


object Arrow {
    def apply[a <: Kind.Function2](implicit i: Arrow[a#apply2]): Arrow[a#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: Arrow[nt#oldtype2]): Arrow[nt#apply2] = new Arrow[nt#apply2] with CategoryProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfCategory: selfCategory = Category.deriving[nt]

        override def arr[b, c](f: b => c): a[b, c] = j.newOf(i.arr(f))
        override def first[b, c, d](f: a[b, c], * : Type[d] = null): a[(b, d), (c, d)] = j.newOf(Lazy(i.first(j.oldOf(Lazy(f)))))
        override def second[b, c, d](f: a[b, c], * : Type[d] = null): a[(d, b), (d, c)] = j.newOf(Lazy(i.second(j.oldOf(Lazy(f)))))

        override def op_***:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[(b, b_), (c, c_)] = j.newOf(i.op_***:(j.oldOf(f))(j.oldOf(g)))
        override def op_&&&:[b, c, c_](f: a[b, c])(g: a[b, c_]): a[b, (c, c_)] = j.newOf(i.op_&&&:(j.oldOf(f))(j.oldOf(g)))
        override def op_^>>:[b, c, d](f: b => c)(a: a[c, d]): a[b, d] = j.newOf(i.op_^>>:(f)(j.oldOf(a)))
        override def op_>>^:[b, c, d](a: a[b, c])(f: c => d): a[b, d] = j.newOf(i.op_>>^:(j.oldOf(a))(f))
        override def op_<<^:[b, c, d](a: a[c, d])(f: b => c): a[b, d] = j.newOf(i.op_<<^:(j.oldOf(a))(f))
        override def op_^<<:[b, c, d](f: c => d)(a: a[b, c]): a[b, d] = j.newOf(i.op_^<<:(f)(j.oldOf(a)))
        override def returnA[b]: a[b, b] = j.newOf(i.returnA[b])
    }

    def weak[nt <: Kind.Newtype2](implicit j: Newtype2[nt#apply2, nt#oldtype2], i: Arrow[nt#apply2]): Arrow[nt#oldtype2] = deriving[Kind.coNewtype2[nt]](j.coNewtype, i)
}
