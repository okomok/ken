

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


trait Floating[a] extends Fractional[a] {
    final val asFloating: Floating[apply0] = this

    // Core
    //
    type pi = a
    def pi: pi

    type exp = a => a
    def exp: exp

    type log = a => a
    def log: log

    type sqrt = a => a
    def sqrt: sqrt = x => x ** 0.5

    type op_** = a => a => a
    def op_** : op_** = x => y => exp(log(x) * y)

    type logBase = a => a => a
    def logBase: logBase = x => y => log(y) / log(x)

    type sin = a => a
    def sin: sin

    type cos = a => a
    def cos: cos

    type tan = a => a
    def tan: tan = x => sin(x) / cos(x)

    type asin = a => a
    def asin: asin

    type acos = a => a
    def acos: acos

    type atan = a => a
    def atan: atan

    type sinh = a => a
    def sinh: sinh

    type cosh = a => a
    def cosh: cosh

    type tanh = a => a
    def tanh: tanh = x => sinh(x) / cosh(x)

    type asinh = a => a
    def asinh: asinh

    type acosh = a => a
    def acosh: acosh

    type atanh = a => a
    def atanh: atanh

    // Operators
    //
    private[ken] sealed class Op_**(x: a) {
        def **(y: a): a = op_**(x)(y)
    }
    final implicit def **(x: a): Op_** = new Op_**(x)
}


trait FloatingProxy[a] extends Floating[a] with FractionalProxy[a] {
    def selfFloating: Floating[a]
    override def selfFractional: Fractional[a] = selfFloating

    override def pi: pi = selfFloating.pi

    override def exp: exp = selfFloating.exp
    override def log: log = selfFloating.log
    override def sqrt: sqrt = selfFloating.sqrt

    override def op_** : op_** = selfFloating.op_**
    override def logBase: logBase = selfFloating.logBase

    override def sin: sin = selfFloating.sin
    override def cos: cos = selfFloating.cos
    override def tan: tan = selfFloating.tan

    override def asin: asin = selfFloating.asin
    override def acos: acos = selfFloating.acos
    override def atan: atan = selfFloating.atan

    override def sinh: sinh = selfFloating.sinh
    override def cosh: cosh = selfFloating.cosh
    override def tanh: tanh = selfFloating.tanh

    override def asinh: asinh = selfFloating.asinh
    override def acosh: acosh = selfFloating.acosh
    override def atanh: atanh = selfFloating.atanh
}


object Floating extends FloatingInstance with FloatingShortcut {
    def apply[a <: Kind.Function0](implicit i: Floating[a#apply0]): Floating[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit i: Floating[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Floating[nt#apply0] = new Floating[nt#apply0] with FractionalProxy[nt#apply0] {
        private type a = nt#apply0
        override val selfFractional = Fractional.deriving[nt](i, j)

        override val pi: pi = j.newOf(i.pi)

        override val exp: exp = x => j.newOf(i.exp(j.oldOf(x)))
        override val log: log = x => j.newOf(i.log(j.oldOf(x)))
        override val sqrt: sqrt = x => j.newOf(i.sqrt(j.oldOf(x)))

        override val op_** : op_** = x => y => j.newOf(i.op_**(j.oldOf(x))(j.oldOf(y)))
        override val logBase: logBase = x => y => j.newOf(i.logBase(j.oldOf(x))(j.oldOf(y)))

        override val sin: sin = x => j.newOf(i.sin(j.oldOf(x)))
        override val cos: cos = x => j.newOf(i.cos(j.oldOf(x)))
        override val tan: tan = x => j.newOf(i.tan(j.oldOf(x)))

        override val asin: asin = x => j.newOf(i.asin(j.oldOf(x)))
        override val acos: acos = x => j.newOf(i.acos(j.oldOf(x)))
        override val atan: atan = x => j.newOf(i.atan(j.oldOf(x)))

        override val sinh: sinh = x => j.newOf(i.sinh(j.oldOf(x)))
        override val cosh: cosh = x => j.newOf(i.cosh(j.oldOf(x)))
        override val tanh: tanh = x => j.newOf(i.tanh(j.oldOf(x)))

        override val asinh: asinh = x => j.newOf(i.asinh(j.oldOf(x)))
        override val acosh: acosh = x => j.newOf(i.acosh(j.oldOf(x)))
        override val atanh: atanh = x => j.newOf(i.atanh(j.oldOf(x)))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Floating[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0, _]): Floating[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](i, j.coNewtype)
}


sealed trait FloatingInstance { this: Floating.type =>
    implicit val ofDouble: Floating[Double] = Double
    implicit val ofFloat: Floating[Float] = Float

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Floating[ot], k: Kind.MethodList.Contains[ds, Real]): Floating[nt] = deriving[Newtype0[nt, ot, _]]
}


sealed trait FloatingShortcut { this: Floating.type =>
}
