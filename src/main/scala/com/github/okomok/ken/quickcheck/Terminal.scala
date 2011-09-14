

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


import RealWorld.IORef


final case class Terminal(override val get: IORef[IO[Unit]]) extends Strong[IORef[IO[Unit]]]
