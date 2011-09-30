

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


import scala.annotation.tailrec
import Random.StdGen


final case class Gen[+a](override val get: StdGen => Int => a) extends NewtypeOf[StdGen => Int => a]


object Gen extends Monad[Gen] with ThisIsInstance {
    // Overrides
    //
    // Functor
    private type f[+a] = Gen[a]
    override def fmap[a, b](f: a => b)(g: f[a]): f[b] = Gen(r => n => f((g.get)(r)(n)))
    // Monad
    private type m[+a] = Gen[a]
    override def `return`[a](x: Lazy[a]): m[a] = Gen(_ => _ => x)
    override def op_>>=[a, b](m: m[a])(k: a => m[b]): m[b] = Gen { r => n =>
        val (r1, r2) = StdGen.split(r)
        val Gen(m_) = k((m.get)(r1)(n))
        m_(r2)(n)
    }

    def unGen[a](g: Gen[a]): StdGen => Int => a = g.get

    def variant[n, a](k: n)(g: Gen[a])(implicit i: Integral[n]): Gen[a] = {
        import i._
        val k_ = k _div_ 2
        lazy val var_ : n => StdGen => StdGen = k => {
            (if (k == k_) id[StdGen] else var_(k_)) `.` (if (even(k)) Pair.fst[StdGen]_ else Pair.snd[StdGen]_) `.` StdGen.split
        }
        Gen(r => n => (g.get)(var_(k)(r))(n))
    }

    def sized[a](f: Int => Gen[a]): Gen[a] = Gen(r => n => (f(n).get)(r)(n))

    def resize[a](n: Int)(g: Gen[a]): Gen[a] = Gen(r => _ => (g.get)(r)(n))

    def choose[a](rng: (a, a))(implicit i: Random[a]): Gen[a] = Gen { r => _ =>
        val (x, _) = i.randomR(rng)(r)
        x
    }

    def promote[m[+_], a](m: m[Gen[a]])(implicit i: Monad[m]): Gen[m[a]] = Gen { r => n =>
        i.liftM((gen: Gen[a]) => (gen.get)(r)(n))(m)
    }

    def `sample'`[a](gen: Gen[a]): IO[List[a]] = {
        for {
            rnd <- Random.newStdGen
        } yield {
            lazy val rnds: StdGen => List[StdGen] = rnd => {
                val (rnd1, rnd2) = StdGen.split(rnd)
                rnd1 :: rnds(rnd2)
            }
            for {
                (r, n) <- List.zip(rnds(rnd))(Int.enumFromThenTo(0)(2)(20))
            } yield (gen.get)(r)(n)
        }
    }

    val sample: Gen[Any] => IO[Unit] = g => {
        for {
            cases <- `sample'`(g)
            * <- IO.sequence_(List.map(IO.print)(cases))
        } yield *
    }

    def suchThat[a](gen: Gen[a])(p: a => Bool): Gen[a] = {
        for {
            mx <- suchThatMaybe(gen)(p)
            * <- mx match {
                case Just(x) => `return`(x)
                case Nothing => sized(n => resize(n+1)(suchThat(gen)(p)))
            }
        } yield *
    }

    def suchThatMaybe[a](gen: Gen[a])(p: a => Bool): Gen[Maybe[a]] = {
        lazy val `try` : Int => Int => Gen[Maybe[a]] = k => {
            case 0 => `return`(Nothing)
            case n => for {
                x <- resize(2*k+n)(gen)
                * <- if (p(x)) `return`(Just(x)) else `try`(k+1)(n-1)
            } yield *
        }
        sized(`try`(0) `.` Int.max(1))
    }

    def oneof[a](gs: List[Gen[a]]): Gen[a] = gs match {
        case Nil => error("QuickCheck.oneof used with empty list")
        case gs => choose(0, List.length(gs) - 1) >>= (gs !! _)
    }

    def frequency[a](xs: List[(Int, Gen[a])]): Gen[a] = {
        val tot: Int = List.sum(List.map(Pair.fst[Int])(xs))

        @tailrec
        def pick(n: Int)(igs: List[(Int, Gen[a])]): Gen[a] = igs match {
            case (k, x) :: xs => {
                if (n <= k) x
                else pick(n-k)(xs)
            }
            case _ => error("QuickCheck.frequency used with empty list")
        }

        choose(1, tot) >>= (pick(_)(xs))
    }

    def elements[a](xs: List[a]): Gen[a] = xs match {
        case Nil => error("QuickCheck.elements used with empty list")
        case xs => fmap(xs !! (_: Int))(choose(0, List.length(xs) - 1))
    }

    def growingElements[a](xs: List[a]): Gen[a] = xs match {
        case Nil => error("QuickCheck.growingElements used with empty list")
        case xs => {
            import Int.{_div_, _max_}
            val k: Int = List.length(xs)
            val mx: Int = 100
            def log_(n: Int): Int = /*Double.round[Int]*/(Double.log(Double.fromIntegral(n))).toInt
            val size: Int => Int = n => ((log_(n) + 1) * k) _div_ log_(mx)
            sized { n => elements(List.take(1 _max_ size(n))(xs)) }
        }
    }

    def listOf[a](gen: Gen[a]): Gen[List[a]] = sized { n =>
        for {
            k <- choose(0, n)
            * <- vectorOf(k)(gen)
        } yield *
    }

    def listOf1[a](gen: Gen[a]): Gen[List[a]] = sized { n =>
        import Int._max_
        for {
            k <- choose(1, 1 _max_ n)
            * <- vectorOf(k)(gen)
        } yield *
    }

    def vectorOf[a](k: Int)(gen: Gen[a]): Gen[List[a]] = sequence( for { _ <- Int.enumFromTo(1)(k) } yield gen )

    def op_><[a](f: Gen[a] => Gen[a])(g: Gen[a] => Gen[a]): Gen[a] => Gen[a] = gen => {
        for {
            n <- Arbitary.arbitary[Int]
            * <- g(Gen.variant(n)(f(gen)))
        } yield *
    }

    private[quickcheck] sealed class Op_><[a](f: Gen[a] => Gen[a]) {
        def ><(g: Gen[a] => Gen[a]): Gen[a] => Gen[a] = op_><(f)(g)
    }
    final implicit def ><[a](f: Gen[a] => Gen[a]): Op_><[a] = new Op_><(f)
}
