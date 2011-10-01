

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

    def byRef[a <: AnyRef]: _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => x eq y
    }

    def byPredicate[a](f: Pair[a, a] => Bool): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => f(x, y)
    }
}


sealed trait EqInstance { this: _Eq.type =>
    implicit def ofDefault[a]: _Eq[a] = new Default[a] {}

    implicit val ofBool: _Eq[Bool] = _Bool
    implicit val ofChar: _Eq[Char] = Char
    implicit val ofDouble: _Eq[Double] = Double
    implicit val ofFloat: _Eq[Float] = Float
    implicit val ofInt: _Eq[Int] = Int
    implicit val ofInteger: _Eq[Integer] = _Integer
    implicit val ofUnit: _Eq[Unit] = Unit

    implicit def ofTuple1[a](implicit i1: _Eq[a]): _Eq[Tuple1[a]] = new _Eq[Tuple1[a]] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1)
    }
    implicit def ofTuple2[a, b](implicit i1: _Eq[a], i2: _Eq[b]): _Eq[(a, b)] = new _Eq[(a, b)] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2)
    }
    implicit def ofTuple3[a, b, c](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c]): _Eq[(a, b, c)] = new _Eq[(a, b, c)] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3)
    }
    implicit def ofTuple4[a, b, c, d](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d]): _Eq[(a, b, c, d)] = new _Eq[(a, b, c, d)] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4)
    }
    implicit def ofTuple5[a, b, c, d, e](implicit i1: _Eq[a], i2: _Eq[b], i3: _Eq[c], i4: _Eq[d], i5: _Eq[e]): _Eq[(a, b, c, d, e)] = new _Eq[(a, b, c, d, e)] {
        override val op_=== : op_=== = x => y => i1.op_===(x._1)(y._1) && i2.op_===(x._2)(y._2) && i3.op_===(x._3)(y._3) && i4.op_===(x._4)(y._4) && i5.op_===(x._5)(y._5)
        override val op_/== : op_/== = x => y => i1.op_/==(x._1)(y._1) || i2.op_/==(x._2)(y._2) || i3.op_/==(x._3)(y._3) || i4.op_/==(x._4)(y._4) || i5.op_/==(x._5)(y._5)
    }

    def ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_=== : op_=== = x => y => i.equiv(x, y)
    }
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