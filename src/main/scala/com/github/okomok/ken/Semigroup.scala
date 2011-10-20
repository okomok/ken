

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2011 Edward Kmett
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Semigroup[a] extends Typeclass0[a] {
    final val asSemigroup: Semigroup[apply0] = this

    // Core
    //
    type op_<>: = a => Lazy[a] => a
    def op_<>: : op_<>:

    type sconcat = List[a] => a
    def sconcat: sconcat = as => {
        lazy val go: a => Lazy[List[a]] => a = b => {
            case !(c :: cs) => b <>: go(c)(cs)
            case !(Nil) => b
        }
        as match {
            case Nil => error("shall be a non-empty list")
            case a :: as => go(a)(as)
        }
    }

    def times1p[n](y0: n)(x0: a)(implicit j: Integral[n]): a = {
        import j._
        lazy val f: a => n => a = x => y => {
            if (even(y)) f(x <>: x)(y _quot_ 2)
            else if (y === 1) x
            else g(x <>: x)((y - 1) _quot_ 2)(x)
        }
        lazy val g: a => n => a => a = x => y => z => {
            if (even(y)) g(x <>: x)(y _quot_ 2)(z)
            else if (y === 1) (x <>: z)
            else g(x <>: x)((y - 1) _quot_ 2)(x <>: z)
        }
        f(x0)(y0 + 1)
    }

    // Extra
    //
    type cycle1 = a => a
    def cycle1: cycle1 = xs => {
        lazy val xs_ : a = xs <>: xs_
        xs_ : a // type-ascription needed (SI-5099)
    }

    type diff = a => Endo[a]
    def diff: diff = x => Endo(op_<>:(x))

    // Operators
    //
    private[ken] sealed class Op_<>:(y: Lazy[a]) {
        def <>:(x: a): a = op_<>:(x)(y)
    }
    final implicit def <>:(y: => a): Op_<>: = new Op_<>:(y)
}


trait SemigroupProxy[a] extends Semigroup[a] {
    def selfSemigroup: Semigroup[a]

    override def op_<>: : op_<>: = selfSemigroup.op_<>:

    override def sconcat: sconcat = selfSemigroup.sconcat
    override def times1p[n](y0: n)(x0: a)(implicit j: Integral[n]): a = selfSemigroup.times1p(y0)(x0)(j)
    override def cycle1: cycle1 = selfSemigroup.cycle1

}


object Semigroup extends SemigroupInstance with SemigroupShortcut {
    def apply[a <: Kind.Function0](implicit i: Semigroup[a#apply0]): Semigroup[a#apply0] = i

    def deriving[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Semigroup[nt#oldtype0]): Semigroup[nt#apply0] = new Semigroup[nt#apply0] {
        private type a = nt#apply0
        override val op_<>: : op_<>: = x => y => j.newOf(i.op_<>:(j.oldOf(x))(j.oldOf(y)))

        override val sconcat: sconcat = as => j.newOf(i.sconcat(List.map((x: a) => j.oldOf(x))(as)))
        override val cycle1: cycle1 = x => j.newOf(i.cycle1(j.oldOf(x)))
    }

    def weak[nt <: Kind.Newtype0](implicit j: Newtype0[nt#apply0, nt#oldtype0, _], i: Semigroup[nt#apply0]): Semigroup[nt#oldtype0] = deriving[Kind.coNewtype0[nt]](j.coNewtype, i)
}


sealed trait SemigroupInstance { this: Semigroup.type =>
    implicit val ofUnit: Semigroup[Unit] = Unit
    implicit def ofFunction[z, b](implicit mb: Semigroup[b]): Semigroup[z => b] = Function._asSemigroup[z, b]
    implicit def ofTuple2[a, b](implicit ma: Semigroup[a], mb: Semigroup[b]): Semigroup[(a, b)] = Tuple2._asSemigroup[a, b]

    implicit def ofNewtype0[nt, ot, ds <: Kind.MethodList](implicit j: Newtype0[nt, ot, ds], i: Semigroup[ot], k: Kind.MethodList.Contains[ds, Semigroup]): Semigroup[nt] = deriving[Newtype0[nt, ot, _]]
}


sealed trait SemigroupShortcut { this: Semigroup.type =>
    def op_<>:[a](x: a)(y: Lazy[a])(implicit i: Semigroup[a]): a = i.op_<>:(x)(y)
    def cycle1[a](xs: a)(implicit i: Semigroup[a]): a = i.cycle1(xs)

    private[ken] class _Op_<>:[a](y: Lazy[a]) {
        def <>:(x: a)(implicit i: Semigroup[a]): a = op_<>:(x)(y)
    }
    implicit def <>:[a](y: => a): _Op_<>:[a] = new _Op_<>:(y)
}

