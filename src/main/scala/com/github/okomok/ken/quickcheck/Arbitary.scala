

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


trait Arbitary[a] extends Typeclass0[a] {
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


object Arbitary extends ArbitaryInstance with ArbitaryShortcut {
    def apply[a <: Kind.Function0](implicit i: Arbitary[a#apply0]): Arbitary[a#apply0] = i
}


trait ArbitaryProxy[a] extends Arbitary[a] {
    def selfArbitary: Arbitary[a]

    override def arbitary: arbitary = selfArbitary.arbitary
    override def shrink: shrink = selfArbitary.shrink

    override def vectorOf: vectorOf = selfArbitary.vectorOf
    override def orderedList(implicit j: Ord[a]): Gen[List[a]] = selfArbitary.orderedList(j)
}


sealed trait ArbitaryInstance { this: Arbitary.type =>
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
                * <- List.sequence( for { _ <- Int.enumFromTo(1)(k) } yield i.arbitary )
            } yield *
        }
        override val shrink: shrink = xs => error("todo")
    }

    implicit val ofInt: Arbitary[Int] = new Arbitary[Int] {
        override val arbitary: arbitary = error("todo")
        override val shrink: shrink = error("todo")
    }

    // TODO
}


sealed trait ArbitaryShortcut { this: Arbitary.type =>
    def arbitary[a](implicit i: Arbitary[a]): Gen[a] = i.arbitary
    def shrink[a](implicit i: Arbitary[a]): a => List[a] = i.shrink
}
