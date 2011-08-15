

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Work around name collision against Ord's EQ.
// Toplevel identifier is case-insensitive under the influence of file-system.

trait _Eq[-a] extends Typeclass { outer =>
    type apply = a
    final val asEq: _Eq[apply] = this

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


trait EqProxy[a] extends _Eq[a] with Proxy {
    override def self: _Eq[a]

    override def op_== = self.op_==
    override def op_/= = self.op_/=
}


object _Eq extends EqInstance {
    def apply[a <: Kind.Function0](implicit i: _Eq[a#apply0]): _Eq[a#apply0] = i

    def deriving[nt <: Kind.Function0, ot <: Kind.Function0](implicit i: _Eq[ot#apply0], j: Newtype0[nt#apply0, ot#apply0]): _Eq[nt#apply0] = new _Eq[nt#apply0] {
        private[this] type a = nt#apply0
        override val op_== : a => a => Bool = x => y => i.op_==(j.old0(x))(j.old0(y))
        override val op_/= : a => a => Bool = x => y => i.op_/=(j.old0(x))(j.old0(y))
    }

    def weak[nt <: Kind.Newtype0](implicit i: _Eq[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): _Eq[nt#oldtype0] = deriving[Kind.const0[nt#oldtype0], nt](i, j.dual)
}


private[ken] trait EqInstance { this: Eq.type =>
    implicit def _ofScalaEquiv[a](implicit i: scala.Equiv[a]): _Eq[a] = new _Eq[a] {
        override val op_== : a => a => Bool = x => y => i.equiv(x, y)
    }

    implicit def _ofScalaOrdering[a](implicit i: scala.Ordering[a]): Ord[a] = new Ord[a] with EqProxy[a] {
        override val self = _ofScalaEquiv(i)
        override val compare: a => a => Ordering = { x => y => i.compare(x, y) match {
            case 0 => EQ
            case s if s < 0 => LT
            case s if s > 0 => GT
        } }
        override val op_< : a => a => Bool = { x => y => i.lt(x, y) }
        override val op_<= : a => a => Bool = { x => y => i.lteq(x, y) }
        override val op_> : a => a => Bool = { x => y => i.gt(x, y) }
        override val op_>= : a => a => Bool = { x => y => i.gteq(x, y) }
        override val max: a => a => a = { x => y => i.max(x, y) }
        override val min: a => a => a = { x => y => i.min(x, y) }
    }

    implicit def _ofScalaNumeric[a](implicit i: scala.Numeric[a]): Ix[a] = new Ix[a] with OrdProxy[a] {
        override val self = _ofScalaOrdering(i)
        override val range: Tuple2[a, a] => List[a] = { case (n, m) =>
            Predef.require(i.lteq(n, m))
            if (i.equiv(n, m)) Nil else n :: range(i.plus(n, i.one), m)
        }
        override val unsafeIndex: Tuple2[a, a] => a => Int = { case (n, _) => k => i.toInt(i.minus(k, n)) }
        override val index: Tuple2[a, a] => a => Int = b => i => {
            if (inRange(b)(i)) unsafeIndex(b)(i) else indexError(b)(i)("Integer")
        }
        override val inRange: Tuple2[a, a] => a => Bool = { case (n, m) => k => i.lteq(n, k) && i.lteq(k, m) }
    }
}