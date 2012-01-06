

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


final case class State(
    terminal: Terminal,
    maxSuccessTests: Int,
    maxDiscardedTests: Int,
    computeSize: Int => Int => Int,

    numSuccessTests: Int,
    numDiscardedTests: Int,
    collected: List[List[(String, Int)]],
    expectedFailure: Bool,
    randomSeed: StdGen,

    isShrinking: Bool,
    numSuccessShrinks: Int,
    numTryShrinks: Int
)


object State {
    val terminal: State => Terminal = _.terminal
    val maxSuccessTests: State => Int = _.maxSuccessTests
    val computeSize: State => Int => Int => Int = _.computeSize
    val numSuccessTests: State => Int = _.numSuccessTests
    val numDiscardedTests: State => Int = _.numDiscardedTests
    val collected: State => List[List[(String, Int)]] = _.collected
    val expectedFailure: State => Bool = _.expectedFailure
    val randomSeed: State => StdGen =_.randomSeed
    val isShrinking: State => Bool = _.isShrinking
    val numSuccessShrinks: State => Int = _.numSuccessShrinks
    val numTryShrinks: State => Int = _.numTryShrinks
}
