

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken



trait ArrowChoice[a[-_, +_]] extends Arrow[a] {
    final val asArrowChoice: ArrowChoice[apply] = this

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

    // Infix
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
    override def self: ArrowChoice[a]

    override def left[b, c, d](f: a[b, c]): a[Either[b, d], Either[c, d]] = self.left(f)
    override def right[b, c, d](f: a[b, c]): a[Either[d, b], Either[d, c]] = self.right(f)
    override def op_+++[b, c, b_, c_](f: a[b, c])(g: a[b_, c_]): a[Either[b, b_], Either[c, c_]] = self.op_+++(f)(g)
    override def op_|||[b, c, d](f: a[b, d])(g: a[c, d]): a[Either[b, c], d] = self.op_|||(f)(g)
}


object ArrowChoice {
    def apply[a <: Kind.Function2](implicit i: ArrowChoice[a#apply]): ArrowChoice[a#apply] = i
}
