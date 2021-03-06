

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


object Modifiers {

    // Blind
    //
    final case class Blind[a](override val old: a) extends NewtypeOf[a] {
        override def toString: JString = "(*)"
    }

    object Blind {
        implicit def _asNewType0[a]: Newtype[Blind[a], a, Eq ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Kind.Nil] = new Newtype[Blind[a], a, Eq ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Kind.Nil] {
            override val newOf: newOf = ot => Blind(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Blind[a]] = new Arbitary[Blind[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Blind(x))(aa.arbitary)
            override val shrink: shrink = { case Blind(x) => for { x_ <- aa.shrink(x) } yield Blind(x_) }
        }
    }

    // Fixed
    //
    final case class Fixed[a](override val old: a) extends NewtypeOf[a]

    object Fixed {
        implicit def _asNewType0[a]: Newtype[Fixed[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] = new Newtype[Fixed[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Fixed(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Fixed[a]] = new Arbitary[Fixed[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Fixed(x))(aa.arbitary)
        }
    }

    // OrderedList
    //
    final case class OrderedList[a](override val old: List[a]) extends NewtypeOf[List[a]]

    object OrderedList {
        implicit def _asNewType0[a]: Newtype[OrderedList[a], List[a], Eq ^:: Ord ^:: Show ^:: Kind.Nil] = new Newtype[OrderedList[a], List[a], Eq ^:: Ord ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => OrderedList(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit ao: Ord[a], aa: Arbitary[a]): Arbitary[OrderedList[a]] = new Arbitary[OrderedList[a]] {
            override val arbitary: arbitary = Gen.fmap((xs: List[a]) => OrderedList(xs))(aa.orderedList)
            override val shrink: shrink = { case OrderedList(xs) => for { xs_ <- Arbitary.shrink(xs) if List.sort(xs_) == List.sort(xs_) } yield OrderedList(xs_) }
        }
    }

    // NonEmptyList
    //
    final case class NonEmptyList[a](override val old: List[a]) extends NewtypeOf[List[a]]

    object NonEmptyList {
        implicit def _asNewType0[a]: Newtype[NonEmptyList[a], List[a], Eq ^:: Ord ^:: Show ^:: Kind.Nil] = new Newtype[NonEmptyList[a], List[a], Eq ^:: Ord ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => NonEmptyList(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit ao: Ord[a], aa: Arbitary[a]): Arbitary[NonEmptyList[a]] = new Arbitary[NonEmptyList[a]] {
            override val arbitary: arbitary = Gen.fmap((xs: List[a]) => NonEmptyList(xs))(Gen.suchThat(Arbitary.arbitary[List[a]])(xs => Bool.not(List.`null`(xs))))
            override val shrink: shrink = { case NonEmptyList(xs) => for { xs_ <- Arbitary.shrink(xs) if Bool.not(List.`null`(xs_)) } yield NonEmptyList(xs_) }
        }
    }

    // Positive
    //
    final case class Positive[a](override val old: a) extends NewtypeOf[a]

    object Positive {
        implicit def _asNewType0[a]: Newtype[Positive[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] = new Newtype[Positive[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Positive(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[Positive[a]] = new Arbitary[Positive[a]] {
            import ao.{>, /==}
            override val arbitary: arbitary = Gen.fmap((x: a) => Positive(am.abs(x)))(Gen.suchThat(aa.arbitary)(_ /== am.fromInteger(0)))
            override val shrink: shrink = { case Positive(x) => for { x_ <- aa.shrink(x) if (x_ > am.fromInteger(0)) } yield Positive(x_) }
        }
    }

    // NonZero
    //
    final case class NonZero[a](override val old: a) extends NewtypeOf[a]

    object NonZero {
        implicit def _asNewType0[a]: Newtype[NonZero[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] = new Newtype[NonZero[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => NonZero(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[NonZero[a]] = new Arbitary[NonZero[a]] {
            import ao.{>, /==}
            override val arbitary: arbitary = Gen.fmap((x: a) => NonZero(x))(Gen.suchThat(aa.arbitary)(_ /== am.fromInteger(0)))
            override val shrink: shrink = { case NonZero(x) => for { x_ <- aa.shrink(x) if (x_ /== am.fromInteger(0)) } yield NonZero(x_) }
        }
    }

    // NonNegative
    //
    final case class NonNegative[a](override val old: a) extends NewtypeOf[a]

    object NonNegative {
        implicit def _asNewType0[a]: Newtype[NonNegative[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] = new Newtype[NonNegative[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => NonNegative(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[NonNegative[a]] = new Arbitary[NonNegative[a]] {
            import ao.{>, >=}
            override val arbitary: arbitary = Gen.frequency { List( (5, Gen.fmap((x: a) => NonNegative(am.abs(x)))(aa.arbitary)), (1, Gen.`return`(NonNegative(am.fromInteger(0)))) ) }
            override val shrink: shrink = { case NonNegative(x) => for { x_ <- aa.shrink(x) if (x_ >= am.fromInteger(0)) } yield NonNegative(x_) }
        }
    }

    // Shrink2
    //
    final case class Shrink2[a](override val old: a) extends NewtypeOf[a]

    object Shrink2 {
        implicit def _asNewType0[a]: Newtype[Shrink2[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] = new Newtype[Shrink2[a], a, Eq ^:: Show ^:: Ord ^:: Num ^:: Integral ^:: Real ^:: Enum ^:: Show ^:: Kind.Nil] {
            override val newOf: newOf = ot => Shrink2(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Shrink2[a]] = new Arbitary[Shrink2[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Shrink2(x))(aa.arbitary)
            override val shrink: shrink = { case Shrink2(x) =>
                val shrink_x = aa.shrink(x)
                ( for { y <- shrink_x } yield Shrink2(y) ) ++:
                ( for { y <- shrink_x; z <- aa.shrink(y) } yield Shrink2(y) )
            }
        }
    }

    // Smart
    //
    final case class Smart[a](i: Int, x: a)

    object Smart {
        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Smart[a]] = new Arbitary[Smart[a]] {
            override val arbitary: arbitary = for { x <- aa.arbitary } yield Smart(0, x)
            override val shrink: shrink = { case Smart(i, x) =>
                val ys = for { (i, y) <- List.zip(Int.enumFrom(0))(aa.shrink(x)) } yield Smart(i, y)
                val i_ = Int.max(0)(i-2)
                lazy val ilv: List[Smart[a]] => List[Smart[a]] => List[Smart[a]] = as => bs => (as, bs) match {
                    case (Nil, bs) => bs
                    case (as, Nil) => as
                    case (a :: as, b :: bs) => a :: b :: ilv(as)(bs)
                }
                ilv(List.take(i_)(ys))(List.drop(i_)(ys))
            }
        }
    }

    // Shrinking
    //
    final case class Shrinking[s, a](s: s, x: a)

    // ShrinkState
    //
    trait ShrinkState[s, a] extends TypeclassLike {
        type shrinkInit = a => s
        def shrinkInit: shrinkInit

        type shrinkState = a => s => List[(a, s)]
        def shrinkState: shrinkState
    }

    object ShrinkState {
        implicit def _asArbitary[s, a](implicit aa: Arbitary[a], as: ShrinkState[s, a]): Arbitary[Shrinking[s, a]] = new Arbitary[Shrinking[s, a]] {
            override val arbitary: arbitary = for { x <- aa.arbitary } yield Shrinking(as.shrinkInit(x), x)
            override val shrink: shrink = { case Shrinking(s, x) => for { (x_, s_) <- as.shrinkState(x)(s) } yield Shrinking(s_, x_) }
        }
    }
}
