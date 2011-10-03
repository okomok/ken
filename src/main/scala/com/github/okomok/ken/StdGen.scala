

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


final class StdGen private[ken] (private val s: Long = java.lang.System.currentTimeMillis) {
    private val rep = new java.util.Random(s)
    override def toString = "StdGen(" + s + ")"
}


object StdGen extends RandomGen[StdGen] with Show.Default[StdGen] with ThisIsInstance {
    def apply(s: Int): StdGen = new StdGen(s)

    // Overrides
    //
    // RandomGen
    override val next: next = g => (g.rep.nextInt, g)
    override val split: split = g => {
        val (t, _) = next(g)
        (new StdGen(g.s - t.toLong), new StdGen(g.s + t.toLong))
    }
}
