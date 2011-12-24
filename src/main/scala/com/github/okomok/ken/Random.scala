

// Copyright Shunsuke Sogame 2011.
//
// Copyright 2004, The University Court of the University of Glasgow.
// All rights reserved.
//
// Copyright (c) 2002 Simon Peyton Jones
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


import RealWorld._


trait Random[a] extends Typeclass[a] {
    final val asRandom: Random[apply0] = this

    // Core
    //
    def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g)
    def random[g](g: g)(implicit i: RandomGen[g]): (a, g)

    def randomRs[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): List[a] = {
        val (x, g_) = randomR(ival)(g)
        x :: randomRs(ival)(g_)
    }

    def randoms[g](g: g)(implicit i: RandomGen[g]): List[a] = {
        val (x, g_) = random(g)
        x :: randoms(g_)
    }

    type randomRIO = Pair[a, a] => IO[a]
    def randomRIO: randomRIO = range => IO.getStdRandom(randomR(range))

    def randomIO: IO[a] = IO.getStdRandom { g => random(g) }
}


trait RandomProxy[a] extends Random[a] {
    type selfRandom = Random[a]
    def selfRandom: selfRandom

    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = selfRandom.randomR(ival)(g)(i)
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = selfRandom.random(g)(i)
    override def randomRs[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): List[a] = selfRandom.randomRs(ival)(g)(i)
    override def randoms[g](g: g)(implicit i: RandomGen[g]): List[a] = selfRandom.randoms(g)(i)
    override def randomRIO: randomRIO = selfRandom.randomRIO
    override def randomIO: IO[a] = selfRandom.randomIO
}


object Random extends RandomInstance with RandomShortcut with RandomDetail


sealed trait RandomInstance { this: Random.type =>
    implicit val _ofBool: Random[Bool] = _Bool
    implicit val _ofChar: Random[Char] = Char
    implicit val _ofInt: Random[Int] = Int
    implicit val _ofInteger: Random[Integer] = _Integer
}


trait RandomShortcut {
    def randomR[a, g](ival: (a, a))(g: g)(implicit ir: Random[a], i: RandomGen[g]): (a, g) = ir.randomR(ival)(g)(i)
    def random[a, g](g: g)(implicit ir: Random[a], i: RandomGen[g]): (a, g) = ir.random(g)(i)
    def randomRs[a, g](ival: (a, a))(g: g)(implicit ir: Random[a], i: RandomGen[g]): List[a] = ir.randomRs(ival)(g)(i)
    def randoms[a, g](g: g)(implicit ir: Random[a], i: RandomGen[g]): List[a] = ir.randoms(g)(i)
    def randomRIO[a](ival: (a, a))(implicit ir: Random[a]): IO[a] = ir.randomRIO(ival)
    def randomIO[a](implicit ir: Random[a]): IO[a] = ir.randomIO
}


private[ken] sealed trait RandomDetail { this: Random.type =>
    private[ken] def randomIvalInteger[g, a](ival: (Integer, Integer))(rng: g)(implicit i: RandomGen[g], j: Num[a]): (a, g) = ival match {
        case (l, h) if l > h => randomIvalInteger(h, l)(rng)
        case (l, h) => {
            import Integer._mod_
            val k: Integer = h - l + 1
            val b: Integer = 2147483561L
            val n: Integer = iLogBase(b)(k)
            lazy val f: Integer => Integer => g => (Integer, g) = n_ => acc => g => {
                if (n_ == 0) {
                    (acc, g)
                } else {
                    val (x, g_) = i.next(g)
                    f(n_ - 1)(Integer.fromIntegral(x) + acc * b)(g_)
                }
            }
            f(n)(1)(rng) match {
                case (v, rng_) => (j.fromInteger(l + (v _mod_ k)), rng_)
            }
        }
    }

    private[ken] def randomIvalDouble[g, a](ival: (Double, Double))(fromDouble: Double => a)(rng: g)(implicit i: RandomGen[g], j: Fractional[a]): (a, g) = ival match {
        case (l, h) if l > h => randomIvalDouble(h, l)(fromDouble)(rng)
        case (l, h) => randomIvalInteger[g, Int](Int.minBound, Int.maxBound)(rng) match {
            case (x, rng_) => {
                import j.{+, *}
                val scaled_x: a = fromDouble((l+h)/2) + fromDouble((h-l) / Double.realToFrac(int32Count)) * j.fromIntegral(x)
                (scaled_x, rng_)
            }
        }
    }

    private val iLogBase: Integer => Integer => Integer = b => i => {
        import Integer._div_
        if (i < b) 1 else (1 + iLogBase(b)(i _div_ b))
    }

    private val int32Count: Integer = Int.toInteger(Int.maxBound) - Int.toInteger(Int.minBound)
    private val stdRange: (Int, Int) = (0, 2147483562)
}
