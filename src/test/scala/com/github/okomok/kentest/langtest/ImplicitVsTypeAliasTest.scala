

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


/*
class ImplicitVsTypeAliasTezt {

    class Monad[m[_]] {
        type For[a] = _For[m, a]
        implicit def toFor[a](m: m[a]): For[a] = throw new Error("todo") // lookup fails
//        implicit def toFor[a](m: m[a]): _For[m, a] = throw new Error("todo") // fine.
    }

    trait _For[m[_], a] {
        def map[b](p: a => b): m[b]
    }

    def useMonad[m[_], a](m: m[a])(implicit i: Monad[m]) {
        import i._

        // value map is not a member of type parameter m[a]
        for {
            x <- m
        } yield x.toString
    }
}

*/
