

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


class ForSomeVsUnapply2 {
    type XXX = (a, a => Unit) forSome { type a }

    // ok
    def foo(x: XXX) {
        x match { case (x, f) => f(x) }
    }

    /* no
    // constructor of type (T1, T2) cannot be uniquely instantiated to expected type Tuple2[?>: Nothing <: Any, ?>: Nothing <: Any => Unit]
    def foo2(x: (XXX, Int)) {
        x match { case ((x, f), _) => f(x) }
    }
    */
}
