

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


trait Random[a] extends Typeclass0[a] {
    final val asRandom: Random[apply0] = this

    // Core
    //
    def randomR[g](rng: (a, a))(g: g)(implicit i: RandomGen[g]): (a, g)
    def random[g](g: g)(implicit i: RandomGen[g]): (a, g)
}


trait RandomProxy[a] extends Random[a] {
    def selfRandom: Random[a]
}


object Random extends RandomInstance with RandomShortcut {

    // StdGen
    //
    final class StdGen private[Random] (private val s: Long = System.currentTimeMillis) {
        private val rep = new java.util.Random(s)
        override def toString = "StdGen(" + s + ")"
    }

    object StdGen extends RandomGen[StdGen] with Show.Of[StdGen] with ThisIsInstance {
        def apply(s: Int): StdGen = new StdGen(s)

        // Overrides
        //
        // RandomGen
        override val next: next = g => (g.rep.nextInt, g)
        override val split: split = g => (new StdGen(g.s + 1), new StdGen(g.s - 1))
    }

    import RealWorld._

    lazy val setStdGen: StdGen => IO[Unit] = sgen => writeIORef(theStdGen)(sgen)
    lazy val getStdGen: IO[StdGen] = readIORef(theStdGen)

    lazy val theStdGen: IORef[StdGen] = IO.unsafePerformIO {
        for {
            rng <- IO.`return`(new StdGen())
            * <- newIORef(rng)
        } yield *
    }

    lazy val newStdGen: IO[StdGen] = atomicModifyIORef(theStdGen)(StdGen.split)
}


sealed trait RandomInstance { this: Random.type =>
}


sealed trait RandomShortcut { this: Random.type =>
}
