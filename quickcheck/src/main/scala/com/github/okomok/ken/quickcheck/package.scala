

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken


package object quickcheck {

    // Exception
    //
    def tryEvaluate[a](x: a): IO[Either[SomeException, a]] = tryEvaluateIO(IO.`return`(x))

    def tryEvaluateIO[a](m: IO[a]): IO[Either[SomeException, a]] = {
        SomeException.`try`(m >>= Exception.evaluate)
    }

    // Property
    //
    type Property = Gen[Prop]
    val Property = _Property

    // Test
    //
    def quickCheck[prop](prop: prop)(implicit i: Testable[prop]): IO[Unit] = i.quickCheck(prop)
    def quickCheckWith[prop](args: Test.Args)(prop: prop)(implicit i: Testable[prop]): IO[Unit] = i.quickCheckWith(args)(prop)
    def quickCheckWithResult[prop](args: Test.Args)(prop: prop)(implicit i: Testable[prop]): IO[Test.Result] = i.quickCheckWithResult(args)(prop)

    implicit def ==>:(b: Bool): Testable._Op_==>: = new Testable._Op_==>:(b)
    implicit def :&:[prop](p1: prop): Testable._Op_:&:[prop] = new Testable._Op_:&:(p1)
}
