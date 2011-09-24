

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
    final case class Blind[a](override val get: a) extends NewtypeOf[a]

    object Blind /*extends Blind_*/ {
        implicit def _asNewType0[a]: Newtype0[Blind[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[Blind[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
            override val newOf: newOf = ot => Blind(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Blind[a]] = new Arbitary[Blind[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Blind(x))(aa.arbitary)
            override val shrink: shrink = { case Blind(x) => for { x_ <- aa.shrink(x) } yield Blind(x_) }
        }
    }

    private[quickcheck] sealed trait Blind_0 { this: Blind.type =>
        implicit def _asOrd[a](implicit an: Ord[a]): Ord[Blind[a]] = Ord.deriving[Blind[a]]
        implicit def _asReal[a](implicit an: Real[a]): Real[Blind[a]] = Real.deriving[Blind[a]]
        implicit def _asEnum[a](implicit an: Enum[a]): Enum[Blind[a]] = Enum.deriving[Blind[a]]
        implicit def _asIntegral[a](implicit an: Integral[a]): Integral[Blind[a]] = Integral.deriving[Blind[a]]
    }

    private[quickcheck] sealed trait Blind_ extends Blind_0 { this: Blind.type =>
        implicit def _asNum[a](implicit an: Num[a]): Num[Blind[a]] = Num.deriving[Blind[a]]
    }


    // Fixed
    //
    final case class Fixed[a](override val get: a) extends NewtypeOf[a]

    object Fixed /*extends Fixed_*/ {
        implicit def _asNewType0[a]: Newtype0[Fixed[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[Fixed[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
            override val newOf: newOf = ot => Fixed(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit aa: Arbitary[a]): Arbitary[Fixed[a]] = new Arbitary[Fixed[a]] {
            override val arbitary: arbitary = Gen.fmap((x: a) => Fixed(x))(aa.arbitary)
        }
    }

    private[quickcheck] sealed trait Fixed_0 { this: Fixed.type =>
        implicit def _asOrd[a](implicit i: Ord[a]): Ord[Fixed[a]] = Ord.deriving[Fixed[a]]
        implicit def _asReal[a](implicit i: Real[a]): Real[Fixed[a]] = Real.deriving[Fixed[a]]
        implicit def _asEnum[a](implicit i: Enum[a]): Enum[Fixed[a]] = Enum.deriving[Fixed[a]]
        implicit def _asIntegral[a](implicit i: Integral[a]): Integral[Fixed[a]] = Integral.deriving[Fixed[a]]
    }

    private[quickcheck] sealed trait Fixed_ extends Fixed_0 { this: Fixed.type =>
        implicit def _asNum[a](implicit i: Num[a]): Num[Fixed[a]] = Num.deriving[Fixed[a]]
    }

    // OrderedList
    //
    final case class OrderedList[a](override val get: List[a]) extends NewtypeOf[List[a]]

    object OrderedList {
        implicit def _asNewType0[a]: Newtype0[OrderedList[a], List[a], Kind.nil] = new Newtype0[OrderedList[a], List[a], Kind.nil] {
            override val newOf: newOf = ot => OrderedList(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit ao: Ord[a], aa: Arbitary[a]): Arbitary[OrderedList[a]] = new Arbitary[OrderedList[a]] {
            override val arbitary: arbitary = Gen.fmap((xs: List[a]) => OrderedList(xs))(aa.orderedList)
            override val shrink: shrink = { case OrderedList(xs) => for { xs_ <- Arbitary.shrink(xs) if List.sort(xs_) == List.sort(xs_) } yield OrderedList(xs_) }
        }

        implicit def _asOrd[a](implicit i: Ord[a]): Ord[OrderedList[a]] = Ord.deriving[OrderedList[a]]
    }

    // NonEmptyList
    //
    final case class NonEmptyList[a](override val get: List[a]) extends NewtypeOf[List[a]]

    object NonEmptyList {
        implicit def _asNewType0[a]: Newtype0[NonEmptyList[a], List[a], Kind.nil] = new Newtype0[NonEmptyList[a], List[a], Kind.nil] {
            override val newOf: newOf = ot => NonEmptyList(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit ao: Ord[a], aa: Arbitary[a]): Arbitary[NonEmptyList[a]] = new Arbitary[NonEmptyList[a]] {
            override val arbitary: arbitary = Gen.fmap((xs: List[a]) => NonEmptyList(xs))(Gen.suchThat(Arbitary.arbitary[List[a]])(xs => Bool.not(List.`null`(xs))))
            override val shrink: shrink = { case NonEmptyList(xs) => for { xs_ <- Arbitary.shrink(xs) if Bool.not(List.`null`(xs_)) } yield NonEmptyList(xs_) }
        }

        implicit def _asOrd[a](implicit i: Ord[a]): Ord[NonEmptyList[a]] = Ord.deriving[NonEmptyList[a]]
    }

    // Positive
    //
    final case class Positive[a](override val get: a) extends NewtypeOf[a]

    object Positive /*extends Positive_*/ {
        implicit def _asNewType0[a]: Newtype0[Positive[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[Positive[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
            override val newOf: newOf = ot => Positive(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[Positive[a]] = new Arbitary[Positive[a]] {
            import ao.{>, /==}
            override val arbitary: arbitary = Gen.fmap((x: a) => Positive(am.abs(x)))(Gen.suchThat(aa.arbitary)(_ /== am.fromInteger(0)))
            override val shrink: shrink = { case Positive(x) => for { x_ <- aa.shrink(x) if (x_ > am.fromInteger(0)) } yield Positive(x_) }
        }
    }

    private[quickcheck] sealed trait Positive_0 { this: Positive.type =>
        implicit def _asOrd[a](implicit i: Ord[a]): Ord[Positive[a]] = Ord.deriving[Positive[a]]
        implicit def _asReal[a](implicit i: Real[a]): Real[Positive[a]] = Real.deriving[Positive[a]]
        implicit def _asEnum[a](implicit i: Enum[a]): Enum[Positive[a]] = Enum.deriving[Positive[a]]
        implicit def _asIntegral[a](implicit i: Integral[a]): Integral[Positive[a]] = Integral.deriving[Positive[a]]
    }

    private[quickcheck] sealed trait Positive_ extends Positive_0 { this: Positive.type =>
        implicit def _asNum[a](implicit i: Num[a]): Num[Positive[a]] = Num.deriving[Positive[a]]
    }

    // NonZero
    //
    final case class NonZero[a](override val get: a) extends NewtypeOf[a]

    object NonZero /*extends NonZero_*/ {
        implicit def _asNewType0[a]: Newtype0[NonZero[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[NonZero[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
            override val newOf: newOf = ot => NonZero(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[NonZero[a]] = new Arbitary[NonZero[a]] {
            import ao.{>, /==}
            override val arbitary: arbitary = Gen.fmap((x: a) => NonZero(x))(Gen.suchThat(aa.arbitary)(_ /== am.fromInteger(0)))
            override val shrink: shrink = { case NonZero(x) => for { x_ <- aa.shrink(x) if (x_ /== am.fromInteger(0)) } yield NonZero(x_) }
        }
    }

    private[quickcheck] sealed trait NonZero_0 { this: NonZero.type =>
        implicit def _asOrd[a](implicit i: Ord[a]): Ord[NonZero[a]] = Ord.deriving[NonZero[a]]
        implicit def _asReal[a](implicit i: Real[a]): Real[NonZero[a]] = Real.deriving[NonZero[a]]
        implicit def _asEnum[a](implicit i: Enum[a]): Enum[NonZero[a]] = Enum.deriving[NonZero[a]]
        implicit def _asIntegral[a](implicit i: Integral[a]): Integral[NonZero[a]] = Integral.deriving[NonZero[a]]
    }

    private[quickcheck] sealed trait NonZero_ extends NonZero_0 { this: NonZero.type =>
        implicit def _asNum[a](implicit i: Num[a]): Num[NonZero[a]] = Num.deriving[NonZero[a]]
    }

    // NonNegative
    //
    final case class NonNegative[a](override val get: a) extends NewtypeOf[a]

    object NonNegative /*extends NonNegative_*/ {
        implicit def _asNewType0[a]: Newtype0[NonNegative[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[NonNegative[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
            override val newOf: newOf = ot => NonNegative(ot)
            override val oldOf: oldOf = nt => nt.get
        }

        implicit def _asArbitary[a](implicit am: Num[a], ao: Ord[a], aa: Arbitary[a]): Arbitary[NonNegative[a]] = new Arbitary[NonNegative[a]] {
            import ao.{>, >=}
            override val arbitary: arbitary = Gen.frequency { List( (5, Gen.fmap((x: a) => NonNegative(am.abs(x)))(aa.arbitary)), (1, Gen.`return`(NonNegative(am.fromInteger(0)))) ) }
            override val shrink: shrink = { case NonNegative(x) => for { x_ <- aa.shrink(x) if (x_ >= am.fromInteger(0)) } yield NonNegative(x_) }
        }
    }

    private[quickcheck] sealed trait NonNegative_0 { this: NonNegative.type =>
        implicit def _asOrd[a](implicit i: Ord[a]): Ord[NonNegative[a]] = Ord.deriving[NonNegative[a]]
        implicit def _asReal[a](implicit i: Real[a]): Real[NonNegative[a]] = Real.deriving[NonNegative[a]]
        implicit def _asEnum[a](implicit i: Enum[a]): Enum[NonNegative[a]] = Enum.deriving[NonNegative[a]]
        implicit def _asIntegral[a](implicit i: Integral[a]): Integral[NonNegative[a]] = Integral.deriving[NonNegative[a]]
    }

    private[quickcheck] sealed trait NonNegative_ extends NonNegative_0 { this: NonNegative.type =>
        implicit def _asNum[a](implicit i: Num[a]): Num[NonNegative[a]] = Num.deriving[NonNegative[a]]
    }

    // Shrink2
    //
    final case class Shrink2[a](override val get: a) extends NewtypeOf[a]

    object Shrink2 /*extends Shrink2_*/ {
        implicit def _asNewType0[a]: Newtype0[Shrink2[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] = new Newtype0[Shrink2[a], a, Num.type :^: Ord.type :^: Real.type :^: Enum.type :^: Integral.type :^: Kind.nil] {
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

    private[quickcheck] sealed trait Shrink2_0 { this: Shrink2.type =>
        implicit def _asOrd[a](implicit i: Ord[a]): Ord[Shrink2[a]] = Ord.deriving[Shrink2[a]]
        implicit def _asReal[a](implicit i: Real[a]): Real[Shrink2[a]] = Real.deriving[Shrink2[a]]
        implicit def _asEnum[a](implicit i: Enum[a]): Enum[Shrink2[a]] = Enum.deriving[Shrink2[a]]
        implicit def _asIntegral[a](implicit i: Integral[a]): Integral[Shrink2[a]] = Integral.deriving[Shrink2[a]]
    }

    private[quickcheck] sealed trait Shrink2_ extends Shrink2_0 { this: Shrink2.type =>
        implicit def _asNum[a](implicit i: Num[a]): Num[Shrink2[a]] = Num.deriving[Shrink2[a]]
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
    trait ShrinkState[s, a] extends Typeclass {
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
