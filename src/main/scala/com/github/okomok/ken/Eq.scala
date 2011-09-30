

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Avoid name collision against `Ord`'s `EQ`.
// (Toplevel identifier is case-insensitive under the influence of file-system.)


trait _Eq[a] extends Typeclass0[a] {
    final val asEq: _Eq[apply0] = this

    // Core
    //
    type op_=== = a => a => Bool
    def op_=== : op_=== = x => y => Bool.not(op_/==(x)(y))

    type op_/== = a => a => Bool
    def op_/== : op_/== = x => y => Bool.not(op_===(x)(y))

    // Operators
    //
    private[ken] sealed class Op_===(x: a) {
        def ===(y: a): Bool = op_===(x)(y)
    }
    final implicit def ===(x: a): Op_=== = new Op_===(x)

    private[ken] sealed class Op_/==(x: a) {
        def /==(y: a): Bool = op_/==(x)(y)
    }
    final implicit def /==(x: a): Op_/== = new Op_/==(x)
}


trait EqProxy[a] extends _Eq[a] {
    def selfEq: _Eq[a]

    override def op_=== : op_=== = selfEq.op_===
    override def op_/== : op_/== = selfEq.op_/==
}


object _Eq extends EqInstance with EqShortcut {
    def apply[a <: Kind.Function0](implicit i: _Eq[a#apply0]): _Eq[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: _Eq[nt#oldtype0]): _Eq[nt#apply0] = new _Eq[nt#apply0] {
        override val op_=== : op_=== = x => y => i.op_===(j.oldOf(x))(j.oldOf(y))
        override val op_/== : op_/== = x => y => i.op_/==(j.oldOf(x))(j.oldOf(y))
    }

    def weak[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: _Eq[nt#apply0]): _Eq[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](j.coNewtype, i)

    trait Default[a] extends _Eq[a] {
        override val op_=== : op_=== = x => y => x == y
        override val op_/== : op_/== = x => y => x != y
    }

    def default[a]: _Eq[a] = new Default[a] {}

    def byRef[a <: AnyRef]: _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => x eq y
    }

    def byPredicate[a](f: Pair[a, a] => Bool): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => f(x, y)
    }
}


sealed trait EqInstance { this: _Eq.type =>
    implicit def ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => i.equiv(x, y)
    }
}


sealed trait EqShortcut { this: _Eq.type =>
    def op_===[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_===(x)(y)
    def op_/==[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_/==(x)(y)

    private[ken] class _Op_===[a](x: a)(implicit i: _Eq[a]) {
        def ===(y: a): Bool = op_===(x)(y)
    }
    implicit def ===[a](x: a)(implicit i: _Eq[a]): _Op_===[a] = new _Op_===(x)

    private[ken] class _Op_/==[a](x: a)(implicit i: _Eq[a]) {
        def /==(y: a): Bool = op_/==(x)(y)
    }
    implicit def /==[a](x: a)(implicit i: _Eq[a]): _Op_/==[a] = new _Op_/==(x)
}