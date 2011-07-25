

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


trait Num[a] extends Klass {
    type apply = a

    final def asNum: Num[a] = this

// Overridables
    def op_+ : a => a => a
    def op_- : a => a => a = { x => y => op_+(x)(negate(y)) }
    def op_* : a => a => a
    def negate: a => a = { x => op_-(fromInt(0))(x) }
    def abs: a => a
    def signum: a => a
    def fromInt: Int => a

// Utilities
    final def subtract: a => a => a = flip(op_-)

// Infix Operators
    sealed class Infix_+(x: a) {
        def +(y: a): a = op_+(x)(y)
    }
    final implicit def +(x: a): Infix_+ = new Infix_+(x)

    sealed class Infix_-(x: a) {
        def -(y: a): a = op_-(x)(y)
    }
    final implicit def -(x: a): Infix_- = new Infix_-(x)

    sealed class Infix_*(x: a) {
        def *(y: a): a = op_*(x)(y)
    }
    final implicit def *(x: a): Infix_* = new Infix_*(x)
}


trait NumProxy[a] extends Num[a] with Proxy {
    override def self: Num[a]
    override def op_+ : a => a => a = self.op_+
    override def op_- : a => a => a = self.op_-
    override def op_* : a => a => a = self.op_*
    override def negate: a => a = self.negate
    override def abs: a => a = self.abs
    override def signum: a => a = self.signum
    override def fromInt: Int => a = self.fromInt
}


object Num {
    def apply[a](implicit i: Num[a]): Num[a] = i

    implicit def ofNumeric[a](implicit i: Numeric[a]): Num[a] = new Num[a] {
        override val op_+ : a => a => a = { x => y => i.plus(x, y) }
        override val op_- : a => a => a = { x => y => i.minus(x, y) }
        override val op_* : a => a => a = { x => y => i.times(x, y) }
        override val negate: a => a = { x => i.negate(x) }
        override val abs: a => a = { x => i.abs(x) }
        override val signum: a => a = { x => fromInt(i.signum(x)) }
        override val fromInt: Int => a = { n => i.fromInt(n) }
    }
}
