

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
    //terminal: Terminal,
    maxSuccessTests: Int,
    maxDiscardTests: Int,
    computeSize: Int => Int => Int,

    numSuccessTests: Int,
    numDiscardedTests: Int,
    collected: List[List[(String, Int)]],
    expectedFailure: Bool,
    randomSeed: Random.StdGen,

    isShrinking: Bool,
    numSuccessShrinks: Int,
    numTryShrinks: Int
)
