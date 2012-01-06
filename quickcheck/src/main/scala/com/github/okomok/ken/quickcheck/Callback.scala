

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


sealed trait Callback extends Up[Callback]

final case class PostTest(f: State => Result => IO[Unit]) extends Callback
final case class PostFinalFailure(f: State => Result => IO[Unit]) extends Callback
