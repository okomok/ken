

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok.kentest


import com.github.okomok.ken._
import Monad.`for`
import List._


class EscapeFromZurgTest extends org.scalatest.junit.JUnit3Suite {

    type Space[m, s] = List[(List[m], s)]

    trait SearchProblem[s, m] {
        def trans(s: s): List[(m, s)]
        def isSolution(x: (List[m], s)): Boolean

        def space(s: s): Space[m, s] = {
            val step: Space[m, s] = for { (m, t) <- trans(s) } yield (m :: Nil, t)
            def expand(ss: Space[m, s]): Space[m, s] = for { (ms, s) <- ss; (ns, t) <- space(s) } yield (ms ::: ns, t)
            step ::: expand(step)
        }
        def solutions(s: s): Space[m, s] = filter(isSolution)(space(s))
    }

    object EscapeFromZurg {

        object Toys extends scala.Enumeration {
            type Toy = Value
            val Buzz, Hamm, Rex, Woody = Value
        }
        import Toys._

        sealed abstract class Pos
        case object L extends Pos
        case object R extends Pos

        type Group = List[Toy]
        type BridgePos = (Pos, Group)
        type Move = Either[Toy, Group]

        val toys: List[Toy] = List(Buzz, Hamm, Rex, Woody)

        def time(t: Toy): Int = t match {
            case Buzz => 5
            case Woody => 10
            case Rex => 20
            case Hamm => 25
        }

        def duration(xs: List[Move]): Int = sum(map(Either.either(time)((g: Group) => maximum(map(time)(g))))(xs))

        def backw(xs: List[Toy]): List[(Move, BridgePos)] = for { x <- xs } yield (Left(x), (L, sort(x :: (toys \\ xs))))

        def forw(xs: List[Toy]): List[(Move, BridgePos)] = for {
            x <- xs; ys = delete(x)(xs); y <- ys if x < y
        } yield (Right(x :: y :: Nil), (R, delete(y)(ys)))

        def solution = Instance.solutions(L, toys)

        implicit object Instance extends SearchProblem[BridgePos, Move] {
            override def trans(s: BridgePos) = s match {
                case (L, l) => forw(l)
                case (R, l) => backw(toys \\ l)
            }
            override def isSolution(s: (List[Move], BridgePos)) = s match {
                case (ms, s) => (s == (R, Nil)) && (duration(ms) <= 60)
            }
        }
    }

    def testTrivial {
        println(EscapeFromZurg.solution)
    }
}
