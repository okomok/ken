

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Real[a] extends Num[a] with Ord[a] {
    final val asReal: Real[apply0] = this

    // Core
    //
    def toRational: a => Rational
}


trait RealProxy[a] extends Real[a] with NumProxy[a] with OrdProxy[a] {
    def selfReal: Real[a]
    override def selfNum: Num[a] = selfReal
    override def selfOrd: Ord[a] = selfReal

    override def toRational: a => Rational = selfReal.toRational
}


object Real extends RealInstance {
    def apply[a <: Kind.Function0](implicit i: Real[a#apply0]): Real[a#apply0] = i
}


sealed trait RealInstance { this: Real.type =>
    implicit val _ofInt: Real[Int] = Int
}
