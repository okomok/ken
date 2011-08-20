

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Integral[a] extends Real[a] with Enum[a] { outer =>
    final val asIntegral: Integral[apply] = this

    // Core
    //
    def quot: a => a => a = n => d => { val (q, _) = quotRem(n)(d); q }
    def rem: a => a => a = n => d => { val (_, r) = quotRem(n)(d); r }
    def div: a => a => a = n => d => { val (q, _) = divMod(n)(d); q }
    def mod: a => a => a = n => d => { val (_, r) = divMod(n)(d); r }

    def quotRem: a => a => (a, a)
    def divMod: a => a => (a, a) = n => d => {
        val qr@(q, r) = quotRem(n)(d)
        if (signum(r) == negate(signum(d))) (q-fromInteger(1), r+d) else qr
    }

    def toInteger: a => Integer

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
}


trait IntegralProxy[a] extends Integral[a] with RealProxy[a] with EnumProxy[a] {
    def selfIntegral: Integral[a] = self
    override def self: Integral[a] = selfIntegral
    override def selfReal: Real[a] = self
    override def selfEnum: Enum[a] = self

    override def quot: a => a => a = self.quot
    override def rem: a => a => a = self.rem
    override def div: a => a => a = self.div
    override def mod: a => a => a = self.mod

    override def quotRem: a => a => (a, a) = self.quotRem
    override def divMod: a => a => (a, a) = self.divMod

    override def toInteger: a => Integer = self.toInteger

    // Enum
    //
    override def succ: a => a = self.succ
    override def pred: a => a = self.pred
    override def toEnum: Int => a = self.toEnum
    override def fromEnum: a => Int = self.fromEnum
    override def enumFrom: a => List[a] = self.enumFrom
    override def enumFromThen: a => a => List[a] = self.enumFromThen
    override def enumFromTo: a => a => List[a] = self.enumFromTo
    override def enumFromThenTo: a => a => a => List[a] = self.enumFromThenTo
}


object Integral {
    def apply[a <: Kind.Function0](implicit i: Integral[a#apply0]): Integral[a#apply0] = i
}
