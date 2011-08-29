

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


trait ArrowChoice[a[-_, +_]] extends Arrow[a] {
    final val asArrowChoice: ArrowChoice[apply2] = this

    // Core
    //
    def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]]

    def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = {
        def mirror[x, y](v: Either[x, y]): Either[y, x] = v match {
            case Left(x) => Right(x)
            case Right(y) => Left(y)
        }
        arr[Either[d, b], Either[b, d]](mirror) >>> left(f) >>> arr(mirror)
    }

    def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = {
        left(f) >>> right(g)
    }

    def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = {
        def utag[x](v: Either[x, x]): x = v match {
            case Left(x) => x
            case Right(y) => y
        }
        f +++ g >>> arr(utag)
    }

    // Operators
    //
    sealed class Op_+++[b, c](f: a[b, c]) {
        def +++[b_, c_](g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = op_+++(f)(g)
    }
    final implicit def +++[b, c](f: a[b, c]): Op_+++[b, c] = new Op_+++[b, c](f)

    sealed class Op_|||[b, d](f: a[b, d]) {
        def |||[c](g: a[c, d]): a[Either[b, c], d] = op_|||(f)(g)
    }
    final implicit def |||[b, d](f: a[b, d]): Op_|||[b, d] = new Op_|||[b, d](f)
}


trait ArrowChoiceProxy[a[-_, +_]] extends ArrowChoice[a] with ArrowProxy[a] {
    def selfArrowChoice: ArrowChoice[a]
    override def selfArrow: Arrow[a] = selfArrowChoice

    override def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = selfArrowChoice.left(f)
    override def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = selfArrowChoice.right(f)
    override def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = selfArrowChoice.op_+++(f)(g)
    override def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = selfArrowChoice.op_|||(f)(g)
}


object ArrowChoice {
    def apply[a <: Kind.Function2](implicit i: ArrowChoice[a#apply2]): ArrowChoice[a#apply2] = i

    def deriving[nt <: Kind.Function2, ot <: Kind.Function2](implicit i: ArrowChoice[ot#apply2], j: Newtype2[nt#apply2, ot#apply2]): ArrowChoice[nt#apply2] = new ArrowChoice[nt#apply2] with ArrowProxy[nt#apply2] {
        private[this] type a[-a, +b] = nt#apply2[a, b]
        override val selfArrow = Arrow.deriving[nt, ot](i, j)

        override def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = j.newOf(Lazy(i.left(j.oldOf(Lazy(f)))))
        override def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = j.newOf(Lazy(i.right(j.oldOf(Lazy(f)))))
        override def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = j.newOf(i.op_+++(j.oldOf(f))(j.oldOf(g)))
        override def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = j.newOf(i.op_|||(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowChoice[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowChoice[nt#oldtype2] = deriving[Kind.quote2[nt#oldtype2], nt](i, j.dual)
}
