

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Avoid name collision against `Ord`'s `EQ`.
// (Toplevel identifier is case-insensitive under the influence of file-system.)


trait _Eq[a] extends Typeclass[a] {
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
    type selfEq = _Eq[a]
    def selfEq: selfEq

    override def op_=== : op_=== = selfEq.op_===
    override def op_/== : op_/== = selfEq.op_/==
}


object _Eq extends EqInstance with EqShortcut {
    def apply[a <: Kind.Function0](implicit i: _Eq[a#apply0]): _Eq[a#apply0] = i

    def deriving[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: _Eq[nt#oldtype]): _Eq[nt#apply0] = new _Eq[nt#apply0] {
        override val op_=== : op_=== = x => y => i.op_===(j.oldOf(x))(j.oldOf(y))
        override val op_/== : op_/== = x => y => i.op_/==(j.oldOf(x))(j.oldOf(y))
    }

    def weak[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: _Eq[nt#apply0]): _Eq[nt#oldtype] = deriving[Kind.coNewtype[nt]](j.coNewtype, i)

    trait Of[a] extends _Eq[a] {
        override val op_=== : op_=== = x => y => x == y
        override val op_/== : op_/== = x => y => x != y
    }

    def byRef[a <: AnyRef]: _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => x eq y
    }

    def byPredicate[a](f: Pair[a, a] => Bool): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => f(x, y)
    }

    def ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => i.equiv(x, y)
    }
}


private[ken] sealed trait EqInstance0 { this: _Eq.type =>
    implicit def of[a]: _Eq[a] = new Of[a] {} // vs `_ofNewtype`
}

sealed trait EqInstance extends EqInstance0 { this: _Eq.type =>
    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: _Eq[ot], k: Kind.MethodList.Contains[ds, Eq]): _Eq[nt] = deriving[Newtype[nt, ot, _]]

    // Tuples
    //
    implicit def _ofTuple1[a](implicit i1: _Eq[a]): _Eq[Tuple1[a]] = new _Eq[Tuple1[a]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1)
    }
    implicit def _ofTuple2[a, b](implicit i1: _Eq[a], i2: _Eq[b]): _Eq[Tuple2[a, b]] = new _Eq[Tuple2[a, b]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2)
    }
    implicit def _ofTuple3[a, b, c](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c]): _Eq[Tuple3[a, b, c]] = new _Eq[Tuple3[a, b, c]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3)
    }
    implicit def _ofTuple4[a, b, c, d](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d]): _Eq[Tuple4[a, b, c, d]] = new _Eq[Tuple4[a, b, c, d]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4)
    }
    implicit def _ofTuple5[a, b, c, d, e](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d], i5: _Eq[e]): _Eq[Tuple5[a, b, c, d, e]] = new _Eq[Tuple5[a, b, c, d, e]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4) && i5.op_===(x._5)(y._5)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4) || i5.op_/==(x._5)(y._5)
    }
}


trait EqShortcut {
    def op_===[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_===(x)(y)
    def op_/==[a](x: a)(y: a)(implicit i: _Eq[a]): Bool = i.op_/==(x)(y)

    private[ken] sealed class _Op_===[a](x: a)(implicit i: _Eq[a]) {
        def ===(y: a): Bool = op_===(x)(y)
    }
    implicit def ===[a](x: a)(implicit i: _Eq[a]): _Op_===[a] = new _Op_===(x)

    private[ken] sealed class _Op_/==[a](x: a)(implicit i: _Eq[a]) {
        def /==(y: a): Bool = op_/==(x)(y)
    }
    implicit def /==[a](x: a)(implicit i: _Eq[a]): _Op_/==[a] = new _Op_/==(x)
}