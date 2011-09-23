

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


import scala.annotation.tailrec


trait Integral[a] extends Real[a] with Enum[a] { outer =>
    final val asIntegral: Integral[apply0] = this

    // Core
    //
    type quot = a => a => a
    def quot: quot = n => d => { val (q, _) = quotRem(n)(d); q }

    type rem = a => a => a
    def rem: rem = n => d => { val (_, r) = quotRem(n)(d); r }

    type div = a => a => a
    def div: div = n => d => { val (q, _) = divMod(n)(d); q }

    type mod = a => a => a
    def mod: mod = n => d => { val (_, r) = divMod(n)(d); r }

    type quotRem = a => a => (a, a)
    def quotRem: quotRem

    type divMod = a => a => (a, a)
    def divMod: divMod = n => d => {
        val qr @ (q, r) = quotRem(n)(d)
        if (signum(r) === negate(signum(d))) (q-1, r+d) else qr
    }

    type toInteger = a => Integer
    def toInteger: toInteger

    // Extra
    //
    type even = a => Bool
    def even: even = n => (n _rem_ 2) === 0

    type odd = a => Bool
    def odd: odd = Bool.not `.` even

    def pow[b](x0: b)(y0: a)(implicit j: Num[b]): b = {
        @tailrec
        def f(x: b)(y: a): b = {
            if (even(y)) f(j.op_*(x)(x))(y _quot_ 2)
            else if (y === 1) x
            else g(j.op_*(x)(x))((y - 1) _quot_ 2)(x)
        }
        @tailrec
        def g(x: b)(y: a)(z: b): b = {
            if (even(y)) g(j.op_*(x)(x))(y _quot_ 2)(z)
            else if (y === 1) j.op_*(x)(z)
            else g(j.op_*(x)(x))((y - 1) _quot_ 2)(j.op_*(x)(z))
        }
        if (y0 < 0) error("Negative exponent")
        else if (y0 === 0) j.fromInteger(1)
        else f(x0)(y0)
    }

    def powpow[b](x: b)(n: a)(implicit j: Fractional[b]): b = if (n >= 0) (x _pow_ n) else j.recip(x _pow_ negate(n))

    // Operators
    //
    private[ken] sealed class Op_quot(n: a) {
        def _quot_(d: a): a = quot(n)(d)
    }
    final implicit def _quot_(n: a): Op_quot = new Op_quot(n)

    private[ken] sealed class Op_rem(n: a) {
        def _rem_(d: a): a = rem(n)(d)
    }
    final implicit def _rem_(n: a): Op_rem = new Op_rem(n)

    private[ken] sealed class Op_div(n: a) {
        def _div_(d: a): a = div(n)(d)
    }
    final implicit def _div_(n: a): Op_div = new Op_div(n)

    private[ken] sealed class Op_mod(n: a) {
        def _mod_(d: a): a = mod(n)(d)
    }
    final implicit def _mod_(n: a): Op_mod = new Op_mod(n)

    private[ken] sealed class Op_pow_[b](x0: b)(implicit j: Num[b]) {
        def _pow_(y0: a): b = pow(x0)(y0)
    }
    final implicit def _pow_[b](x0: b)(implicit j: Num[b]): Op_pow_[b] = new Op_pow_(x0)

    // Convenience
    //
    type toInt = a => Int
    final val toInt: toInt = n => toInteger(n).toInt

    object FromInt {
        def unapply(n: a): Option[Int] = Some(toInt(n))
    }
}


trait IntegralProxy[a] extends Integral[a] with RealProxy[a] with EnumProxy[a] {
    def selfIntegral: Integral[a]
    override def selfReal: Real[a] = selfIntegral
    override def selfEnum: Enum[a] = selfIntegral

    override def quot: quot = selfIntegral.quot
    override def rem: rem = selfIntegral.rem
    override def div: div = selfIntegral.div
    override def mod: mod = selfIntegral.mod

    override def quotRem: quotRem = selfIntegral.quotRem
    override def divMod: divMod = selfIntegral.divMod

    override def toInteger: toInteger = selfIntegral.toInteger

    override def even: even = selfIntegral.even
    override def odd: odd = selfIntegral.odd

    override def pow[b](x0: b)(y0: a)(implicit j: Num[b]): b = selfIntegral.pow(x0)(y0)(j)
    override def powpow[b](x: b)(n: a)(implicit j: Fractional[b]): b = selfIntegral.powpow(x)(n)(j)
}


object Integral extends IntegralInstance with IntegralShortcut {
    def apply[a <: Kind.Function0](implicit i: Integral[a#apply0]): Integral[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: Integral[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): Integral[nt#apply0] = new Integral[nt#apply0] with RealProxy[nt#apply0] with EnumProxy[nt#apply0] {
        private type a = nt#apply0
        override val selfReal = Real.deriving[nt, ot]
        override val selfEnum = Enum.deriving[nt, ot]

        override def quot: quot = x => y => j.newOf(i.quot(j.oldOf(x))(j.oldOf(y)))
        override def rem: rem = x => y => j.newOf(i.rem(j.oldOf(x))(j.oldOf(y)))
        override def div: div = x => y => j.newOf(i.div(j.oldOf(x))(j.oldOf(y)))
        override def mod: mod = x => y => j.newOf(i.mod(j.oldOf(x))(j.oldOf(y)))

        override def quotRem: quotRem = x => y => i.quotRem(j.oldOf(x))(j.oldOf(y)) match { case (q, r) => (j.newOf(q), j.newOf(r)) }
        override def divMod: divMod = x => y => i.divMod(j.oldOf(x))(j.oldOf(y)) match { case (d, v) => (j.newOf(d), j.newOf(v)) }

        override def toInteger: toInteger = x => i.toInteger(j.oldOf(x))

        override def even: even = x => i.even(j.oldOf(x))
        override def odd: odd = x => i.odd(j.oldOf(x))

        override def pow[b](x0: b)(y0: a)(implicit bn: Num[b]): b = i.pow(x0)(j.oldOf(y0))(bn)
        override def powpow[b](x: b)(n: a)(implicit bf: Fractional[b]): b = i.powpow(x)(j.oldOf(n))(bf)
    }

    def weak[nt <: Kind.Newtype0](implicit i: Integral[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Integral[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
}


sealed trait IntegralInstance { this: Integral.type =>
    implicit val ofInt: Integral[Int] = Int
    implicit val ofInteger: Integral[Integer] = _Integer

    implicit def ofScalaIntegral[a](implicit i: scala.math.Integral[a]): Integral[a] = new Integral[a] with NumProxy[a] with OrdProxy[a] with EnumProxy[a] {
        override val selfNum = Num.ofScalaNumeric(i)
        override val selfOrd = Ord.ofScalaOrdering(i)
        override val selfEnum = Enum.ofScalaNumeric(i)
        // Real
        override val toRational: toRational = x => Ratio(toInteger(x), 1)
        // Integral
        override val quot: quot = x => y => i.quot(x, y)
        override val rem: rem = x => y => i.rem(x, y)
        override val quotRem: quotRem = x => y => (i.quot(x, y), i.rem(x, y))
        override val toInteger: toInteger = i.toInt
    }
}


sealed trait IntegralShortcut { this: Integral.type =>
    def quot[a](n: a)(d: a)(implicit i: Integral[a]): a = i.quot(n)(d)
    def rem[a](n: a)(d: a)(implicit i: Integral[a]): a = i.rem(n)(d)
    def div[a](n: a)(d: a)(implicit i: Integral[a]): a = i.div(n)(d)
    def mod[a](n: a)(d: a)(implicit i: Integral[a]): a = i.mod(n)(d)

    def quotRem[a](n: a)(d: a)(implicit i: Integral[a]): (a, a) = i.quotRem(n)(d)
    def divMod[a](n: a)(d: a)(implicit i: Integral[a]): (a, a) = i.divMod(n)(d)

    def toInteger[a](n: a)(implicit i: Integral[a]): Integer = i.toInteger(n)

    def even[a](n: a)(implicit i: Integral[a]): Bool = i.even(n)
    def odd[a](n: a)(implicit i: Integral[a]): Bool = i.odd(n)

    def pow[a, b](x0: b)(y0: a)(implicit i: Integral[a], j: Num[b]): b = i.pow(x0)(y0)
    def powpow[a, b](x: b)(n: a)(implicit i: Integral[a], j: Fractional[b]): b = i.powpow(x)(n)

    private[ken] class _Op_quot[a](n: a)(implicit i: Integral[a]) {
        def _quot_(d: a): a = quot(n)(d)
    }
    implicit def _quot_[a](n: a)(implicit i: Integral[a]): _Op_quot[a] = new _Op_quot(n)

    private[ken] class _Op_rem[a](n: a)(implicit i: Integral[a]) {
        def _rem_(d: a): a = rem(n)(d)
    }
    implicit def _rem_[a](n: a)(implicit i: Integral[a]): _Op_rem[a] = new _Op_rem(n)

    private[ken] class _Op_div[a](n: a)(implicit i: Integral[a]) {
        def _div_(d: a): a = div(n)(d)
    }
    implicit def _div_[a](n: a)(implicit i: Integral[a]): _Op_div[a] = new _Op_div(n)

    private[ken] class _Op_mod[a](n: a)(implicit i: Integral[a]) {
        def _mod_(d: a): a = mod(n)(d)
    }
    implicit def _mod_[a](n: a)(implicit i: Integral[a]): _Op_mod[a] = new _Op_mod(n)

    private[ken] class _Op_pow_[b](x0: b)(implicit j: Num[b]) {
        def _pow_[a](y0: a)(implicit i: Integral[a]): b = pow(x0)(y0)
    }
    implicit def _pow_[b](x0: b)(implicit j: Num[b]): _Op_pow_[b] = new _Op_pow_(x0)

    def toInt[a](n: a)(implicit i: Integral[a]): Int = i.toInt(n)
}
