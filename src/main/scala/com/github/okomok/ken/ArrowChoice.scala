

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
    def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]]

    def right[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[d, b], Either[d, c]] = {
        def mirror[x, y](v: Either[x, y]): Either[y, x] = v match {
            case Left(x) => Right(x)
            case Right(y) => Left(y)
        }
        arr(mirror[d, b]) >>>: left(f, Type[d]) >>>: arr(mirror[c, d])
    }

    def op_+++:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = {
        left(f, Type[b_]) >>>: right(g, Type[c])
    }

    def op_|||:[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = {
        def utag(v: Either[d, d]): d = v match {
            case Left(x) => x
            case Right(y) => y
        }
        f +++: g >>>: arr(utag)
    }

    // Operators
    //
    private[ken] sealed class Op_+++:[b_, c_](g: a[b_, c_]) {
        def +++:[b, c](f: a[b, c]): a[Either[b, b_], Either[c, c_]] = op_+++:(f)(g)
    }
    final implicit def +++:[b_, c_](g: a[b_, c_]): Op_+++:[b_, c_] = new Op_+++:(g)

    private[ken] sealed class Op_|||:[c, d](g: a[c, d]) {
        def |||:[b](f: a[b, d]): a[Either[b, c], d] = op_|||:(f)(g)
    }
    final implicit def |||:[c, d](g: a[c, d]): Op_|||:[c, d] = new Op_|||:(g)
}


trait ArrowChoiceProxy[a[-_, +_]] extends ArrowChoice[a] with ArrowProxy[a] {
    def selfArrowChoice: ArrowChoice[a]
    override def selfArrow: Arrow[a] = selfArrowChoice

    override def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = selfArrowChoice.left(f)
    override def right[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[d, b], Either[d, c]] = selfArrowChoice.right(f)
    override def op_+++:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = selfArrowChoice.op_+++:(f)(g)
    override def op_|||:[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = selfArrowChoice.op_|||:(f)(g)
}


object ArrowChoice {
    def apply[a <: Kind.Function2](implicit i: ArrowChoice[a#apply2]): ArrowChoice[a#apply2] = i

    def deriving[nt <: Kind.Newtype2](implicit i: ArrowChoice[nt#oldtype2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowChoice[nt#apply2] = new ArrowChoice[nt#apply2] with ArrowProxy[nt#apply2] {
        private type a[-a, +b] = nt#apply2[a, b]
        override val selfArrow = Arrow.deriving[nt]

        override def left[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[b, d], Either[c, d]] = j.newOf(Lazy(i.left(j.oldOf(Lazy(f)))))
        override def right[b, c, d](f: a[b, c], * : Type[d] = null): a[Either[d, b], Either[d, c]] = j.newOf(Lazy(i.right(j.oldOf(Lazy(f)))))
        override def op_+++:[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = j.newOf(i.op_+++:(j.oldOf(f))(j.oldOf(g)))
        override def op_|||:[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = j.newOf(i.op_|||:(j.oldOf(f))(j.oldOf(g)))
    }

    def weak[nt <: Kind.Newtype2](implicit i: ArrowChoice[nt#apply2], j: Newtype2[nt#apply2, nt#oldtype2]): ArrowChoice[nt#oldtype2] = deriving[Kind.coNewtype2[nt]](i, j.coNewtype)
}
