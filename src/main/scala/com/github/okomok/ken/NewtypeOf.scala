

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


import scala.annotation.unchecked.uncheckedVariance // Why needed?


// Up is workaround: https://issues.scala-lang.org/browse/SI-4225
trait NewtypeOf[+a] extends Up[NewtypeOf[a]] with Kind.Newtype0 {
    override type apply0 = this.type
    override type oldtype0 = a @uncheckedVariance

    def get: a

    final def run: a = get
    final def app: a = get
/*
    Rejected.
    final def apply[b, c](b: b)(implicit ev: a <:< Function[b, c]): c = ev(get)(b)
    final def apply[b, c, d](b: b)(c: c)(implicit ev: a <:< Function[b, c => d]): d = ev(get)(b)(c)
    final def apply[b, c, d, e](b: b)(c: c)(d: d)(implicit ev: a <:< Function[b, c => d => e]): e = ev(get)(b)(c)(d)
*/
}


trait NewtypeOfProxy[+a] extends NewtypeOf[a] {
    def selfNewtype: NewtypeOf[a]

    override def get: a = selfNewtype.get
}


object NewtypeOf extends Newtype0Of_ {
    def apply[a](a: a): NewtypeOf[a] = new NewtypeOf[a] {
        override def get: a = a
    }
}


private[ken] sealed trait Newtype0Of_0 { this: NewtypeOf.type =>
    implicit def _asOrd[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Ord[ot], k: Kind.MethodList.Contains[ds, Real]): Ord[nt] = Ord.deriving[Newtype0[nt, ot, _]]
    implicit def _asEnum[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Enum[ot], k: Kind.MethodList.Contains[ds, Enum]): Enum[nt] = Enum.deriving[Newtype0[nt, ot, _]]
    implicit def _asBounded[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Bounded[ot], k: Kind.MethodList.Contains[ds, Bounded]): Bounded[nt] = Bounded.deriving[Newtype0[nt, ot, _]]
}

private[ken] sealed trait Newtype0Of_1 extends Newtype0Of_0 { this: NewtypeOf.type =>
    implicit def _asNum[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Num[ot], k: Kind.MethodList.Contains[ds, Num]): Num[nt] = Num.deriving[Newtype0[nt, ot, _]]
    implicit def _asReal[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Real[ot], k: Kind.MethodList.Contains[ds, Real]): Real[nt] = Real.deriving[Newtype0[nt, ot, _]]
}

private[ken] sealed trait Newtype0Of_2 extends Newtype0Of_1 { this: NewtypeOf.type =>
    implicit def _asIx[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Ix[ot], k: Kind.MethodList.Contains[ds, Ix]): Ix[nt] = Ix.deriving[Newtype0[nt, ot, _]]
    implicit def _asFractional[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Fractional[ot], k: Kind.MethodList.Contains[ds, Real]): Fractional[nt] = Fractional.deriving[Newtype0[nt, ot, _]]
    implicit def _asIntegral[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Integral[ot], k: Kind.MethodList.Contains[ds, Integral]): Integral[nt] = Integral.deriving[Newtype0[nt, ot, _]]
}

private[ken] sealed trait Newtype0Of_3 extends Newtype0Of_2 { this: NewtypeOf.type =>
    implicit def _asRealFrac[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: RealFrac[ot], k: Kind.MethodList.Contains[ds, RealFrac]): RealFrac[nt] = RealFrac.deriving[Newtype0[nt, ot, _]]
}

private[ken] sealed trait Newtype0Of_  extends Newtype0Of_3 { this: NewtypeOf.type =>
    implicit def _asFloating[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: Floating[ot], k: Kind.MethodList.Contains[ds, Real]): Floating[nt] = Floating.deriving[Newtype0[nt, ot, _]]
    implicit def _asRealFloat[nt, ot, ds <: Kind.MethodList](implicit i: Newtype0[nt, ot, ds], j: RealFloat[ot], k: Kind.MethodList.Contains[ds, RealFloat]): RealFloat[nt] = RealFloat.deriving[Newtype0[nt, ot, _]]
}
