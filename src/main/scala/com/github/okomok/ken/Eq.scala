

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


// Avoid name collision against `Ord`'s `EQ`.
// (Toplevel identifier is case-insensitive under the influence of file-system.)


trait _Eq[-a] extends Typeclass[a] {
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


trait EqProxy[-a] extends _Eq[a] {
    def selfEq: _Eq[a]

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

    trait Default[a] extends _Eq[a] {
        override val op_=== : op_=== = x => y => x == y
        override val op_/== : op_/== = x => y => x != y
    }

    def byRef[a <: AnyRef]: _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => x eq y
    }

    def byPredicate[a](f: Pair[a, a] => Bool): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => f(x, y)
    }
}


private[ken] sealed trait EqInstance0 { this: _Eq.type =>
    val ofAny: _Eq[Any] = new Default[Any] {}

    implicit def _ofDefault[a]: _Eq[a] = ofAny // vs `_ofNewtype`
}

private[ken] sealed trait EqInstance1 extends EqInstance0 { this: _Eq.type =>
    // Primitives
    //
    implicit val _ofBool: _Eq[Bool] = _Bool
    implicit val _ofChar: _Eq[Char] = Char
    implicit val _ofDouble: _Eq[Double] = Double
    implicit val _ofFloat: _Eq[Float] = Float
    implicit val _ofInt: _Eq[Int] = Int
    implicit val _ofInteger: _Eq[Integer] = _Integer
    implicit val _ofUnit: _Eq[Unit] = Unit

    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: _Eq[ot], k: Kind.MethodList.Contains[ds, Eq]): _Eq[nt] = deriving[Newtype[nt, ot, _]]

    // Products
    //
    implicit def _ofProduct1[a, ds <: Kind.MethodList](implicit i1: _Eq[a], k: Kind.MethodList.Contains[ds, Eq]): _Eq[Product1[a] with Deriving[ds]] = new _Eq[Product1[a] with Deriving[ds]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1)
    }
    implicit def _ofProduct2[a, b, ds <: Kind.MethodList](implicit i1: _Eq[a], i2: _Eq[b], k: Kind.MethodList.Contains[ds, Eq]): _Eq[Product2[a, b] with Deriving[ds]] = new _Eq[Product2[a, b] with Deriving[ds]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2)
    }
    implicit def _ofProduct3[a, b, c, ds <: Kind.MethodList](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], k: Kind.MethodList.Contains[ds, Eq]): _Eq[Product3[a, b, c] with Deriving[ds]] = new _Eq[Product3[a, b, c] with Deriving[ds]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3)
    }
    implicit def _ofProduct4[a, b, c, d, ds <: Kind.MethodList](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d], k: Kind.MethodList.Contains[ds, Eq]): _Eq[Product4[a, b, c, d] with Deriving[ds]] = new _Eq[Product4[a, b, c, d] with Deriving[ds]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4)
    }
    implicit def _ofProduct5[a, b, c, d, e, ds <: Kind.MethodList](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d], i5: _Eq[e], k: Kind.MethodList.Contains[ds, Eq]): _Eq[Product5[a, b, c, d, e] with Deriving[ds]] = new _Eq[Product5[a, b, c, d, e] with Deriving[ds]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4) && i5.op_===(x._5)(y._5)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4) || i5.op_/==(x._5)(y._5)
    }

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

    def ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => i.equiv(x, y)
    }
}

sealed trait EqInstance extends EqInstance1 { this: Eq.type =>
    implicit val _ofNothing: Eq[Nothing] with HighPriority = new Eq[Nothing] with HighPriority {}
}


sealed trait EqShortcut { this: _Eq.type =>
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