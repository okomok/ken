

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2011 Edward Kmett
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Semigroup[a] extends Typeclass[a] {
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


object Semigroup extends SemigroupInstance with SemigroupShortcut with SemigroupType {
    def apply[a <: Kind.Function0](implicit i: Semigroup[a#apply0]): Semigroup[a#apply0] = i

    def deriving[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Semigroup[nt#oldtype]): Semigroup[nt#apply0] = new Semigroup[nt#apply0] {
        private type a = nt#apply0
        override val op_<>: : op_<>: = x => y => j.newOf(i.op_<>:(j.oldOf(x))(j.oldOf(y)))

        override val sconcat: sconcat = as => j.newOf(i.sconcat(List.map((x: a) => j.oldOf(x))(as)))
        override val cycle1: cycle1 = x => j.newOf(i.cycle1(j.oldOf(x)))
    }

    def weak[nt <: Kind.Newtype](implicit j: Newtype[nt#apply0, nt#oldtype, _], i: Semigroup[nt#apply0]): Semigroup[nt#oldtype] = deriving[Kind.coNewtype[nt]](j.coNewtype, i)
}


sealed trait SemigroupInstance { this: Semigroup.type =>
    implicit val _ofUnit: Semigroup[Unit] = Unit
    implicit def _ofFunction[z, b](implicit mb: Semigroup[b]): Semigroup[z => b] = Function._asSemigroup[z, b]
    implicit def _ofTuple2[a, b](implicit ma: Semigroup[a], mb: Semigroup[b]): Semigroup[(a, b)] = Tuple2._asSemigroup[a, b]

    implicit def _ofNewtype[nt, ot, ds <: Kind.MethodList](implicit j: Newtype[nt, ot, ds], i: Semigroup[ot], k: Kind.MethodList.Contains[ds, Semigroup]): Semigroup[nt] = deriving[Newtype[nt, ot, _]]
}


trait SemigroupShortcut {
    def op_<>:[a](x: a)(y: Lazy[a])(implicit i: Semigroup[a]): a = i.op_<>:(x)(y)
    def sconcat[a](xs: List[a])(implicit i: Semigroup[a]): a = i.sconcat(xs)
    def times1p[a, n](y0: n)(x0: a)(implicit i: Semigroup[a], j: Integral[n]): a = i.times1p(y0)(x0)(j)

    def cycle1[a](xs: a)(implicit i: Semigroup[a]): a = i.cycle1(xs)

    private[ken] class _Op_<>:[a](y: Lazy[a]) {
        def <>:(x: a)(implicit i: Semigroup[a]): a = op_<>:(x)(y)
    }
    implicit def <>:[a](y: => a): _Op_<>:[a] = new _Op_<>:(y)
}


sealed trait SemigroupType { this: Semigroup.type =>

    // Min
    //
    final case class Min[a](override val old: a) extends NewtypeOf[a]

    object Min {
        implicit def _asNewtype[a]: Newtype[Min[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] = new Newtype[Min[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Min(ot)
            override val oldOf: oldOf = nt => nt.old
        }

        implicit def _asSemigroup[z](implicit i: Ord[z]): Semigroup[Min[z]] = new Semigroup[Min[z]] {
            private type a = Min[z]
            override val op_<>: : op_<>: = a => b => Min(i.min(a.old)(b.old))
            override def times1p[n](n: n)(a: a)(implicit j: Integral[n]): a = a
        }

        implicit def _asMonoid[z](implicit i: Ord[z], j: Bounded[z]): Monoid[Min[z]] = new Monoid[Min[z]] with SemigroupProxy[Min[z]] {
            override val selfSemigroup = _asSemigroup(i)
            override val mempty: mempty = Min(j.maxBound)
            override val mappend = op_<>:
        }
    }

    // Max
    //
    final case class Max[a](override val old: a) extends NewtypeOf[a]

    object Max {
        implicit def _asNewtype[a]: Newtype[Max[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] = new Newtype[Max[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Max(ot)
            override val oldOf: oldOf = nt => nt.old
        }

        implicit def _asSemigroup[z](implicit i: Ord[z]): Semigroup[Max[z]] = new Semigroup[Max[z]] {
            private type a = Max[z]
            override val op_<>: : op_<>: = a => b => Max(i.max(a.old)(b.old))
            override def times1p[n](n: n)(a: a)(implicit j: Integral[n]): a = a
        }

        implicit def _asMonoid[z](implicit i: Ord[z], j: Bounded[z]): Monoid[Max[z]] = new Monoid[Max[z]] with SemigroupProxy[Max[z]] {
            override val selfSemigroup = _asSemigroup(i)
            override val mempty: mempty = Max(j.minBound)
            override val mappend = op_<>:
        }
    }

    // These are slightly different from those of Monoid.
    //

    // First
    //
    final case class First[a](override val old: a) extends NewtypeOf[a]

    object First {
        implicit def _asNewtype[a]: Newtype[First[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] = new Newtype[First[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => First(ot)
            override val oldOf: oldOf = nt => nt.old
        }

        implicit def _asSemigroup[z]: Semigroup[First[z]] = new Semigroup[First[z]] {
            private type a = First[z]
            override val op_<>: : op_<>: = a => _ => a
            override def times1p[n](n: n)(a: a)(implicit j: Integral[n]): a = a
        }
    }

    // Last
    //
    final case class Last[a](override val old: a) extends NewtypeOf[a]

    object Last {
        implicit def _asNewtype[a]: Newtype[Last[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] = new Newtype[Last[a], a, Eq ^:: Ord ^:: Bounded ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Last(ot)
            override val oldOf: oldOf = nt => nt.old
        }

        implicit def _asSemigroup[z]: Semigroup[Last[z]] = new Semigroup[Last[z]] {
            private type a = Last[z]
            override val op_<>: : op_<>: = _ => b => b
            override def times1p[n](n: n)(a: a)(implicit j: Integral[n]): a = a
        }
    }
}
