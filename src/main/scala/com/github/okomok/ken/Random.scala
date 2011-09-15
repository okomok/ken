

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


trait Random[a] extends Typeclass0[a] {
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
    def randomRIO: randomRIO = range => Random.getStdRandom(randomR(range))

    def randomIO: IO[a] = Random.getStdRandom { g => random(g) }
}


trait RandomProxy[a] extends Random[a] {
    def selfRandom: Random[a]

    override def randomR[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g) = selfRandom.randomR(ival)(g)(i)
    override def random[g](g: g)(implicit i: RandomGen[g]): (a, g) = selfRandom.random(g)(i)
    override def randomRs[g](ival: (a, a))(g: g)(implicit i: RandomGen[g]): List[a] = selfRandom.randomRs(ival)(g)(i)
    override def randoms[g](g: g)(implicit i: RandomGen[g]): List[a] = selfRandom.randoms(g)(i)
    override def randomRIO: randomRIO = selfRandom.randomRIO
    override def randomIO: IO[a] = selfRandom.randomIO
}


object Random extends RandomInstance with RandomShortcut {

    // StdGen
    //
    final class StdGen private[Random] (private val s: Long = java.lang.System.currentTimeMillis) {
        private val rep = new java.util.Random(s)
        override def toString = "StdGen(" + s + ")"
    }

    object StdGen extends RandomGen[StdGen] with Show.Of[StdGen] with ThisIsInstance {
        def apply(s: Int): StdGen = new StdGen(s)

        // Overrides
        //
        // RandomGen
        override val next: next = g => (g.rep.nextInt, g)
        override val split: split = g => (new StdGen(g.s + 1), new StdGen(g.s - 1)) // TODO
    }

    val setStdGen: StdGen => IO[Unit] = sgen => writeIORef(theStdGen)(sgen)
    val getStdGen: IO[StdGen] = readIORef(theStdGen)

    val theStdGen: IORef[StdGen] = IO.unsafePerformIO {
        for {
            rng <- IO.`return`(new StdGen())
            * <- newIORef(rng)
        } yield *
    }

    val newStdGen: IO[StdGen] = atomicModifyIORef(theStdGen)(StdGen.split)

    def getStdRandom[a](f: StdGen => (a, StdGen)): IO[a] = {
        val swap: Pair[a, StdGen] => (StdGen, a) = { case (v, g) => (g, v) }
        atomicModifyIORef(theStdGen)(swap `.` f)
    }
}


sealed trait RandomInstance { this: Random.type =>
    implicit val ofInt: Random[Int] = Int
}


sealed trait RandomShortcut { this: Random.type =>
    def randomR[a, g](ival: (a, a))(g: g)(implicit ir: Random[a], i: RandomGen[g]): (a, g) = ir.randomR(ival)(g)(i)
    def random[a, g](g: g)(implicit ir: Random[a], i: RandomGen[g]): (a, g) = ir.random(g)(i)
    def randomRs[a, g](ival: (a, a))(g: g)(implicit ir: Random[a], i: RandomGen[g]): List[a] = ir.randomRs(ival)(g)(i)
    def randoms[a, g](g: g)(implicit ir: Random[a], i: RandomGen[g]): List[a] = ir.randoms(g)(i)
    def randomRIO[a](ival: (a, a))(implicit ir: Random[a]): IO[a] = ir.randomRIO(ival)
    def randomIO[a](implicit ir: Random[a]): IO[a] = ir.randomIO
}
