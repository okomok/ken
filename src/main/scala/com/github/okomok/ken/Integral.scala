

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
    def odd: odd = not `.` even

    // Operators
    //
    sealed class Op_quot(x: a) {
        def _quot_(y: a): a = quot(x)(y)
    }
    final implicit def _quot_(x: a): Op_quot = new Op_quot(x)

    sealed class Op_rem(x: a) {
        def _rem_(y: a): a = rem(x)(y)
    }
    final implicit def _rem_(x: a): Op_rem = new Op_rem(x)

    sealed class Op_div(x: a) {
        def _div_(y: a): a = div(x)(y)
    }
    final implicit def _div_(x: a): Op_div = new Op_div(x)

    sealed class Op_mod(x: a) {
        def _mod_(y: a): a = mod(x)(y)
    }
    final implicit def _mod_(x: a): Op_mod = new Op_mod(x)

    // Convenience
    //
    final val toInt: a => Int = x => toInteger(x).toInt

    object FromInt {
        def unapply(x: a): Option[Int] = Some(toInt(x))
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
}


object Integral extends IntegralInstance {
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
