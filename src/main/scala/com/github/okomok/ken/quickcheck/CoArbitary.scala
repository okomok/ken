

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


trait CoArbitary[a] extends Typeclass[a] {
    final val asCoArbitary: CoArbitary[a] = this

    // Core
    //
    def coarbitary[c](f: a)(gen: Gen[c]): Gen[c]
}


trait CoArbitaryProxy[a] extends CoArbitary[a] {
    def selfCoArbitary: CoArbitary[a]

    override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = selfCoArbitary.coarbitary(f)(gen)
}


object CoArbitary extends CoArbitaryInstance with CoArbitaryShortcut {
    def apply[a <: Kind.Function0](implicit i: CoArbitary[a#apply0]): CoArbitary[a#apply0] = i

    def coarbitaryIntegral[a, b](x: a)(g: Gen[b])(implicit i: Integral[a]): Gen[b] = Gen.variant(x)(g)

    def coarbitaryReal[a, b](x: a)(g: Gen[b])(implicit i: Real[a]): Gen[b] = coarbitary(i.toRational(x))(g)

    def coarbitaryShow[a, b](x: a)(g: Gen[b])(implicit i: Show[a]): Gen[b] = coarbitary(Show.show(x))(g)
}


sealed trait CoArbitaryInstance { outer: CoArbitary.type =>
    implicit def ofFunction[x, y](implicit i: Arbitary[x], j: CoArbitary[y]): CoArbitary[x => y] = new CoArbitary[x => y] {
        type a = x => y
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = for {
            xs <- Arbitary.arbitary[List[x]]
        } {
            outer.coarbitary(List.map(f)(xs))(gen)
        }
    }

    implicit val ofUnit: CoArbitary[Unit] = new CoArbitary[Unit] {
        type a = Unit
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = gen
    }

    implicit val ofBool: CoArbitary[Bool] = new CoArbitary[Bool] {
        type a = Bool
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case False => Gen.variant(0)(gen)
            case True => Gen.variant(-1)(gen)
        }
    }

    implicit def ofMaybe[x](implicit i: CoArbitary[x]): CoArbitary[Maybe[x]] = new CoArbitary[Maybe[x]] {
        type a = Maybe[x]
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case Nothing => Gen.variant(0)(gen)
            case Just(x) => Gen.variant(-1)(i.coarbitary(x)(gen))
        }
    }

    implicit def ofEither[x, y](implicit i: CoArbitary[x], j: CoArbitary[y]): CoArbitary[Either[x, y]] = new CoArbitary[Either[x, y]] {
        type a = Either[x, y]
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case Left(x) => Gen.variant(0)(i.coarbitary(x)(gen))
            case Right(y) => Gen.variant(-1)(j.coarbitary(y)(gen))
        }
    }

    implicit def ofList[x](implicit i: CoArbitary[x]): CoArbitary[List[x]] = new CoArbitary[List[x]] {
        type a = List[x]
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case Nil => Gen.variant(0)(gen)
            case x :: xs => Gen.variant(-1)(outer.coarbitary(x, xs.!)(gen))
        }
    }

    implicit def ofRatio[x](implicit i: CoArbitary[x], j: Integral[x]): CoArbitary[Ratio[x]] = new CoArbitary[Ratio[x]] {
        type a = Ratio[x]
        override def coarbitary[c](r: a)(gen: Gen[c]): Gen[c] = outer.coarbitary(Ratio.numerator(r), Ratio.denominator(r))(gen)
    }

    implicit def ofTuple2[x1, x2](implicit i1: CoArbitary[x1], i2: CoArbitary[x2]): CoArbitary[(x1, x2)] = new CoArbitary[(x1, x2)] {
        type a = (x1, x2)
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case (x1, x2) => Gen.op_><[c](i1.coarbitary(x1))(i2.coarbitary(x2))(gen)
        }
    }

    implicit def ofTuple3[x1, x2, x3](implicit i1: CoArbitary[x1], i2: CoArbitary[x2], i3: CoArbitary[x3]): CoArbitary[(x1, x2, x3)] = new CoArbitary[(x1, x2, x3)] {
        type a = (x1, x2, x3)
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = f match {
            case (x1, x2, x3) => Gen.op_><[c](Gen.op_><[c](i1.coarbitary(x1))(i2.coarbitary(x2)))(i3.coarbitary(x3))(gen)
        }
    }

    implicit val ofInteger: CoArbitary[Integer] = new CoArbitary[Integer] {
        type a = Integer
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = coarbitaryIntegral(f)(gen)
    }

    implicit val ofInt: CoArbitary[Int] = new CoArbitary[Int] {
        type a = Int
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = coarbitaryIntegral(f)(gen)
    }

    implicit val ofChar: CoArbitary[Char] = new CoArbitary[Char] {
        type a = Char
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = coarbitaryIntegral(Char.ord(f))(gen)
    }

    implicit val ofFloat: CoArbitary[Float] = new CoArbitary[Float] {
        type a = Float
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = coarbitaryReal(f)(gen)
    }

    implicit val ofDouble: CoArbitary[Double] = new CoArbitary[Double] {
        type a = Double
        override def coarbitary[c](f: a)(gen: Gen[c]): Gen[c] = coarbitaryReal(f)(gen)
    }
}


sealed trait CoArbitaryShortcut { this: CoArbitary.type =>
    def coarbitary[a, c](f: a)(gen: Gen[c])(implicit i: CoArbitary[a]): Gen[c] = i.coarbitary(f)(gen)
}
