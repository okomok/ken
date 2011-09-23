

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


class ForSomeVsUnapply {

    class X[e]

    case class Wrap(rep: (e, X[e]) forSome { type e })

    def makeWrap: Wrap = throw new Error("todo")
    def useRep[e](rep: (e, X[e])) = ()

    val Wrap(rep) = makeWrap
/*
[error] type mismatch;
[error]  found   : (Any, ForSomeVsUnapply.this.X[_1]) where type _1
[error]  required: (Any, ForSomeVsUnapply.this.X[Any])
[error]     useRep(rep) // error
[error]            ^
[error] one error found
*/
   // useRep(rep) // error

    useRep(makeWrap.rep) // ok
}

