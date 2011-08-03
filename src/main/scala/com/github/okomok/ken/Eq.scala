

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Work around name collision against Ord's EQ.
// Toplevel identifier is case-insensitive under the influence of file-system.

trait _Eq[-a] extends TypeClass {
    type apply = a
    final def asEq: _Eq[apply] = this

    // Core
    //
    def op_== : a => a => Bool = x => y => not(op_/=(x)(y))
    def op_/= : a => a => Bool = x => y => not(op_==(x)(y))

    // Infix
    //
    sealed class Infix_/=(x: a) {
        def /=(y: a): Bool = op_/=(x)(y)
    }
    final implicit def /=(x: a): Infix_/= = new Infix_/=(x)
}


trait EqProxy[-a] extends _Eq[a] with Proxy {
    override def self: _Eq[a]

    override def op_== = self.op_==
    override def op_/= = self.op_/=
}


object _Eq extends EqInstance {
    def apply[a](implicit i: _Eq[a]): _Eq[a] = i
}


trait EqInstance { this: Eq.type =>
    implicit def ofEquiv[a](implicit i: scala.math.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_== : a => a => Bool = x => y => i.equiv(x, y)
    }
}