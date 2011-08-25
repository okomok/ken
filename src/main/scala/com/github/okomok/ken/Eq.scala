

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Work around name collision against Ord's EQ.
// Toplevel identifier is case-insensitive under the influence of file-system.

trait _Eq[a] extends Typeclass0[a] {
    final val asEq: _Eq[apply0] = this

    // Core
    //
    def op_=== : a => a => Bool = x => y => not(op_/==(x)(y))
    def op_/== : a => a => Bool = x => y => not(op_===(x)(y))

    // Operators
    //
    sealed class Op_===(x: a) {
        def ===(y: a): Bool = op_===(x)(y)
    }
    final implicit def ===(x: a): Op_=== = new Op_===(x)

    sealed class Op_/==(x: a) {
        def /==(y: a): Bool = op_/==(x)(y)
    }
    final implicit def /==(x: a): Op_/== = new Op_/==(x)
}


trait EqProxy[a] extends _Eq[a] {
    def selfEq: _Eq[a]

    override def op_=== : a => a => Bool = selfEq.op_===
    override def op_/== : a => a => Bool = selfEq.op_/==
}


object _Eq extends EqInstance {
    def apply[a <: Kind.Function0](implicit i: _Eq[a#apply0]): _Eq[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: _Eq[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): _Eq[nt#apply0] = new _Eq[nt#apply0] {
        private[this] type a = nt#apply0
        override val op_=== : a => a => Bool = x => y => i.op_===(j.oldOf(x))(j.oldOf(y))
        override val op_/== : a => a => Bool = x => y => i.op_/==(j.oldOf(x))(j.oldOf(y))
    }

    def weak[nt <: Kind.Newtype0](implicit i: _Eq[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): _Eq[nt#oldtype0] = deriving[Kind.const[nt#oldtype0], nt](i, j.dual)
}


private[ken] trait EqInstance { this: Eq.type =>
    implicit val _ofInt: Eq[Int] = Int

    implicit def _ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_=== : a => a => Bool = x => y => i.equiv(x, y)
    }
}