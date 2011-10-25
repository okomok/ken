

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


trait Arbitary[a] extends Typeclass[a] {
    final val asArbitary: Arbitary[a] = this

    // Core
    //
    type arbitary = Gen[a]
    def arbitary: arbitary = error("no default generator")

    type shrink = a => List[a]
    def shrink: shrink = _ => Nil

    // Extra
    //
    type vectorOf = Int => Gen[List[a]]
    def vectorOf: vectorOf = k => Gen.vectorOf(k)(arbitary)

    def orderedList(implicit j: Ord[a]): Gen[List[a]] = Gen.fmap((xs: List[a]) => List.sort(xs))(Arbitary.ofList(this).arbitary)
}


trait ArbitaryProxy[a] extends Arbitary[a] {
    def selfArbitary: Arbitary[a]

    override def arbitary: arbitary = selfArbitary.arbitary
    override def shrink: shrink = selfArbitary.shrink

    override def vectorOf: vectorOf = selfArbitary.vectorOf
    override def orderedList(implicit j: Ord[a]): Gen[List[a]] = selfArbitary.orderedList(j)
}


object Arbitary extends ArbitaryInstance with ArbitaryShortcut {
    def apply[a <: Kind.Function0](implicit i: Arbitary[a#apply0]): Arbitary[a#apply0] = i

    def sizedIntegral[a](implicit i: Num[a]): Gen[a] = Gen.sized { n =>
        val n_ = Int.toInteger(n)
        Gen.fmap(i.fromInteger)(Gen.choose(-n_, n_))
    }

    def sizedFractional[a](implicit i: Fractional[a]): Gen[a] = Gen.sized { n =>
        val precision: Integer = 9999999999999L
        val n_ = Int.toInteger(n)
        for {
            a <- Gen.choose((-n_) * precision, n_ * precision)
            b <- Gen.choose(Int.toInteger(1), precision)
        } yield i.fromRational(Ratio(a, b))
    }

    def boundedIntegral[a](implicit i: Bounded[a], j: Integral[a]): Gen[a] = {
        val mn = i.minBound
        val mx = i.maxBound
        for {
            n <- Gen.choose(j.toInteger(mn), j.toInteger(mx))
        } yield j.fromInteger(n)
    }

    def boundedRandom[a](implicit i: Bounded[a], j: Random[a]): Gen[a] = Gen.choose(i.minBound, i.maxBound)

    lazy val sizedBoundedInt: Gen[Int] = Gen.sized { s =>
        import Integer._
        val mn = Int.minBound
        val mx = Int.maxBound
        val k: Integer = Int.toInteger(2) _pow_ ((Int.toInteger(s)*2) _div_ 5)
        for {
            n <- Gen.choose((Int.toInteger(mn) _max_ (-k), Int.toInteger(mx) _min_ k))
        } yield Int.fromInteger(n)
    }

    def shrinkList[a](shr: a => List[a])(xs: List[a]): List[List[a]] = {
        val removeChunks: List[a] => List[List[a]] = xs => {
            lazy val rem: Int => List[a] => List[List[a]] = {
                case 0 => _ => Nil
                case 1 => _ => List(Nil)
                case n => xs => {
                    import Int._div_
                    import Bool.not

                    val n1 = n _div_ 2
                    val xs1 = List.take(n1)(xs)
                    val n2 = n - n1
                    val xs2 = List.drop(n1)(xs)
                    lazy val ilv: List[List[a]] => List[List[a]] => List[List[a]] = xs => ys => (xs, ys) match {
                        case (Nil, ys) => ys
                        case (xs, Nil) => xs
                        case (x :: xs, y :: ys) => x :: y :: ilv(xs)(ys)
                    }

                    xs1 :: xs2 :: (ilv
                    ( for { xs1_ <- rem(n1)(xs1) if not(List.`null`(xs1_)) } yield (xs1_ ++: xs2) )
                    ( for { xs2_ <- rem(n2)(xs2) if not(List.`null`(xs2_)) } yield (xs1 ++: xs2_) ) )
                }
            }

            rem(List.length(xs))(xs)
        }

        lazy val shrinkOne: List[a] => List[List[a]] = {
            case Nil => Nil
            case x :: xs => {
                ( for { x_ <- shr(x) } yield (x_ :: xs.!) ) ++:
                ( for { xs_ <- shrinkOne(xs) } yield (x :: xs_) )
            }
        }

        removeChunks(xs) ++: shrinkOne(xs)
    }

    lazy val shrinkNothing: Any => List[Nothing] = _ => Nil

    def shrinkIntegral[a](a: a)(implicit i: Integral[a]): List[a] = List.nub {
        import i._
        val op_<< : a => a => Bool = x => y => abs(x) < abs(y)
        ( for { x <- List(a) if x < 0 } yield (-x) ) ++:
        ( for { x_ <- List.takeWhile(op_<<(_: a)(a))(0 :: ( for { i <- List.tail(List.iterate((_: a) _quot_ 2)(a)) } yield (a - i) )) } yield x_ )
    }

    def shrinkRealFrac[a](a: a)(implicit i: RealFrac[a]): List[a] = List.nub {
        import i._
        val op_<< : a => a => Bool = x => y => abs(x) < abs(y)
        ( for { x <- List(a) if x < 0 } yield (-x) ) ++:
        ( for { x_ <- List(fromInteger(truncate[Integer](a))) if op_<<(x_)(a) } yield x_ )
    }
}


sealed trait ArbitaryInstance { this: Arbitary.type =>
    implicit def ofFunction[a, b](implicit i: CoArbitary[a], j: Arbitary[b]): Arbitary[a => b] = new Arbitary[a => b] {
        override val arbitary: arbitary = {
            implicit val fm = Monad[Function.apply[a]]
            Gen.promote[fm.apply, b]((x: a) => i.coarbitary(x)(j.arbitary))
        }
    }

    implicit val ofUnit: Arbitary[Unit] = new Arbitary[Unit] {
        override val arbitary: arbitary = Gen.`return`()
    }

    implicit val ofBool: Arbitary[Bool] = new Arbitary[Bool] {
        override val arbitary: arbitary = Gen.choose(False, True)
    }

    implicit def ofMaybe[a](implicit i: Arbitary[a]): Arbitary[Maybe[a]] = new Arbitary[Maybe[a]] {
        override val arbitary: arbitary = Gen.frequency( List( (1, Gen.`return`(Nothing.of[a])), (3, Gen.liftM((a: a) => Just(a).up)(i.arbitary)) ) )
        override val shrink: shrink = {
            case Just(x) => Nothing.of[a] :: ( for { x_ <- i.shrink(x) } yield Just(x_).up )
            case _ => Nil
        }
    }

    implicit def ofEither[a, b](implicit i: Arbitary[a], j: Arbitary[b]): Arbitary[Either[a, b]] = new Arbitary[Either[a, b]] {
        override lazy val arbitary: arbitary = Gen.oneof( List( Gen.liftM((a: a) => Left(a).of[a, b])(i.arbitary), Gen.liftM((b: b) => Right(b).of[a, b])(j.arbitary) ) )
        override lazy val shrink: shrink = {
            case Left(x) => for { x_ <- i.shrink(x) } yield Left(x_)
            case Right(x) => for { y_ <- j.shrink(x) } yield Right(y_)
        }
    }

    implicit def ofList[a](implicit i: Arbitary[a]): Arbitary[List[a]] = new Arbitary[List[a]] {
        override val arbitary: arbitary = Gen.sized { n =>
            for {
                k <- Gen.choose(0, n)
            } {
                List.sequence( for { _ <- Int.enumFromTo(1)(k) } yield i.arbitary )
            }
        }
        override val shrink: shrink = xs => shrinkList(i.shrink)(xs)
    }

    implicit def ofRatio[a](implicit i: Integral[a]): Arbitary[Ratio[a]] = new Arbitary[Ratio[a]] {
        override val arbitary: arbitary = sizedFractional[Ratio[a]]
        override val shrink: shrink = shrinkRealFrac[Ratio[a]]
    }

    implicit def ofTuple2[a1, a2](implicit i1: Arbitary[a1], i2: Arbitary[a2]): Arbitary[(a1, a2)] = new Arbitary[(a1, a2)] {
        override val arbitary: arbitary = Gen.liftM2((x: a1) => (y: a2) => (x, y))(i1.arbitary)(i2.arbitary)
        override val shrink: shrink = { case (x, y) =>
            ( for { x_ <- i1.shrink(x) } yield (x_, y) ) ++:
            ( for { y_ <- i2.shrink(y) } yield (x, y_) )
        }
    }

    implicit def ofTuple3[a1, a2, a3](implicit i1: Arbitary[a1], i2: Arbitary[a2], i3: Arbitary[a3]): Arbitary[(a1, a2, a3)] = new Arbitary[(a1, a2, a3)] {
        override val arbitary: arbitary = Gen.liftM3((x: a1) => (y: a2) => (z: a3) => (x, y, z))(i1.arbitary)(i2.arbitary)(i3.arbitary)
        override val shrink: shrink = { case (x, y, z) =>
            ( for { x_ <- i1.shrink(x) } yield (x_, y, z) ) ++:
            ( for { y_ <- i2.shrink(y) } yield (x, y_, z) ) ++:
            ( for { z_ <- i3.shrink(z) } yield (x, y, z_) )
        }
    }

    implicit val ofInteger: Arbitary[Integer] = new Arbitary[Integer] {
        override val arbitary: arbitary = sizedIntegral[Integer]
        override val shrink: shrink = shrinkIntegral[Integer]
    }

    implicit val ofInt: Arbitary[Int] = new Arbitary[Int] {
        override val arbitary: arbitary = sizedBoundedInt
        override val shrink: shrink = shrinkIntegral[Int]
    }

    implicit val ofChar: Arbitary[Char] = new Arbitary[Char] {
        override val arbitary: arbitary = Gen.fmap(Char.chr)(Gen.oneof(List(Gen.choose(0, 127), Gen.choose(0, 255))))
        override val shrink: shrink = c => {
            import Bool.not
            import Ord.<

            val stamp: Char => (Bool, Bool, Bool, Bool, Bool, Char) = a =>
                ( not(Char.isLower(a)), not(Char.isUpper(a)), not(Char.isDigit(a)), not(a == ' '), not(Char.isSpace(a)), a )
            val op_<< : Char => Char => Bool = a => b => stamp(a) < stamp(b)

            List.filter(op_<<(_: Char)(c)) {
                List.nub {
                    List('a', 'b', 'c') ++:
                    ( for { _c <- List(c) if Char.isUpper(c) } yield _c ) ++:
                    List('A', 'B', 'C') ++:
                    List('1', '2', '3') ++:
                    List(' ', '\n')
                }
            }
        }
    }

    implicit val ofFloat: Arbitary[Float] = new Arbitary[Float] {
        override val arbitary: arbitary = sizedFractional[Float]
        override val shrink: shrink = shrinkRealFrac[Float]
    }

    implicit val ofDouble: Arbitary[Double] = new Arbitary[Double] {
        override val arbitary: arbitary = sizedFractional[Double]
        override val shrink: shrink = shrinkRealFrac[Double]
    }
}


sealed trait ArbitaryShortcut { this: Arbitary.type =>
    def arbitary[a](implicit i: Arbitary[a]): Gen[a] = i.arbitary
    def shrink[a](x: a)(implicit i: Arbitary[a]): List[a] = i.shrink(x)

    def vectorOf[a](k: Int)(implicit i: Arbitary[a]): Gen[List[a]] = i.vectorOf(k)
    def orderedList[a](implicit i: Arbitary[a], j: Ord[a]): Gen[List[a]] = i.orderedList
}
