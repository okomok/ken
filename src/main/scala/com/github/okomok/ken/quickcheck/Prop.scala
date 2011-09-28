

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


final case class Prop(override val get: Rose[IO[Result]]) extends NewtypeOf[Rose[IO[Result]]]


object Prop extends Testable[Prop] with ThisIsInstance {
    val unProp: Prop => Rose[IO[Result]] = p => p.get

    // Overrides
    //
    // Testable
    override val property: property = x => Gen.`return`(x)
}
