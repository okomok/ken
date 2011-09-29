

// Copyright Shunsuke Sogame 2011.
//
// Copyright (c) 2000-2006, Koen Claessen
// Copyright (c) 2006, Bjorn Bringert
//
// Distributed under the New BSD license.


package com.github.okomok
package ken
package quickcheck


// type Property = Gen[Prop]


object Property {

    // Lifting
    //
    val liftBool: Bool => Property = b => liftResult {
        Result.result.copy(ok = Just(b), reason = if (b) "" else "Falsifiable")
    }

    val liftResult: Result => Property = r => liftIOResult(IO.`return`(r))

    val liftIOResult: IO[Result] => Property = m => {
        val wrap: IO[Result] => IO[Result] = m => IO.fmap(Either.either(Result.exception[SomeException])(id[Result]))(tryEvaluateIO(m))
        liftRoseIOResult(Rose.`return`(wrap(m)))
    }

    val liftRoseIOResult: Rose[IO[Result]] => Property = t => Gen.`return`(Prop(t))
}
