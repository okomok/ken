

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

    // Operators
    //
    sealed class Op_quot(n: a) {
        def _quot_(d: a): a = quot(n)(d)
    }
    final implicit def _quot_(n: a): Op_quot = new Op_quot(n)

    sealed class Op_rem(n: a) {
        def _rem_(d: a): a = rem(n)(d)
    }
    final implicit def _rem_(n: a): Op_rem = new Op_rem(n)

    sealed class Op_div(n: a) {
        def _div_(d: a): a = div(n)(d)
    }
    final implicit def _div_(n: a): Op_div = new Op_div(n)

    sealed class Op_mod(n: a) {
        def _mod_(d: a): a = mod(n)(d)
    }
    final implicit def _mod_(n: a): Op_mod = new Op_mod(n)

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
}


object Integral extends IntegralInstance with IntegralShortcut {
    def apply[a <: Kind.Function0](implicit i: Integral[a#apply0]): Integral[a#apply0] = i
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

    sealed class _Op_quot[a](n: a)(implicit i: Integral[a]) {
        def _quot_(d: a): a = quot(n)(d)
    }
    implicit def _quot_[a](n: a)(implicit i: Integral[a]): _Op_quot[a] = new _Op_quot(n)

    sealed class _Op_rem[a](n: a)(implicit i: Integral[a]) {
        def _rem_(d: a): a = rem(n)(d)
    }
    implicit def _rem_[a](n: a)(implicit i: Integral[a]): _Op_rem[a] = new _Op_rem(n)

    sealed class _Op_div[a](n: a)(implicit i: Integral[a]) {
        def _div_(d: a): a = div(n)(d)
    }
    implicit def _div_[a](n: a)(implicit i: Integral[a]): _Op_div[a] = new _Op_div(n)

    sealed class _Op_mod[a](n: a)(implicit i: Integral[a]) {
        def _mod_(d: a): a = mod(n)(d)
    }
    implicit def _mod_[a](n: a)(implicit i: Integral[a]): _Op_mod[a] = new _Op_mod(n)

    def toInt[a](n: a)(implicit i: Integral[a]): Int = i.toInt(n)
}
