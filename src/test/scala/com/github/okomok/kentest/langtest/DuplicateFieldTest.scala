

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


// hmmm, I can't reproduce...
class DuplicateFieldTezt {

    class abc(why: String) extends StaticAnnotation

    @abc("foo")
    trait A {
        @abc("foo")
        def hello = ()
    }

    @abc("bar")
    trait B

    class C extends A with B
}
