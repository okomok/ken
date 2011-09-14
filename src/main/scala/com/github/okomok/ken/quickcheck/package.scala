

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
        import IO.>>=
        SomeException.`try`(m >>= Exception.evaluate)
    }

}
