

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

    def deriving[nt <: Kind.Newtype0](implicit i: Real[nt#oldtype0], j: Newtype0[nt#apply0, nt#oldtype0]): Real[nt#apply0] = new Real[nt#apply0] with NumProxy[nt#apply0] with OrdProxy[nt#apply0] {
        override val selfNum = Num.deriving[nt]
        override val selfOrd = Ord.deriving[nt]

        override val toRational: toRational = x => i.toRational(j.oldOf(x))
    }

    def weak[nt <: Kind.Newtype0](implicit i: Real[nt#apply0], j: Newtype0[nt#apply0, nt#oldtype0]): Real[nt#oldtype0] = deriving[Kind.dualNewtype0[nt]](i, j.dual)
}


sealed trait RealInstance { this: Real.type =>
    implicit val ofDouble: Real[Double] = Double
    implicit val ofFloat: Real[Float] = Float
    implicit val ofInt: Real[Int] = Int
    implicit val ofInteger: Real[Integer] = _Integer
}
