

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Real[a] extends Num[a] with Ord[a] {
    final val asReal: Real[apply0] = this

    // Core
    //
    type toRational = a => Rational
    def toRational: toRational
}


trait RealProxy[a] extends Real[a] with NumProxy[a] with OrdProxy[a] {
    def selfReal: Real[a]
    override def selfNum: Num[a] = selfReal
    override def selfOrd: Ord[a] = selfReal

    override def toRational: toRational = selfReal.toRational
}


object Real extends RealInstance {
    def apply[a <: Kind.Function0](implicit i: Real[a#apply0]): Real[a#apply0] = i

    def deriving[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Real[nt#oldtype]): Real[nt#apply0] = new Real[nt#apply0] with NumProxy[nt#apply0] with OrdProxy[nt#apply0] {
        override val selfNum = Num.deriving[nt]
        override val selfOrd = Ord.deriving[nt]

        override val toRational: toRational = x => i.toRational(j.oldOf(x))
    }

    def weak[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Real[nt#apply0]): Real[nt#oldtype] = deriving[Kind.coNewtype[nt]](j.coNewtype, i)
}


sealed trait RealInstance { this: Real.type =>
    implicit val _ofDouble: Real[Double] = Double
    implicit val _ofFloat: Real[Float] = Float
    implicit val _ofInt: Real[Int] = Int
    implicit val _ofInteger: Real[Integer] = _Integer

    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: Real[ot], k: Kind.MethodList.Contains[ds, Real]): Real[nt] = deriving[Newtype[nt, ot, _]]
}
