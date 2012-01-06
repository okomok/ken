

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok.kentest.langtest


class TypeLambdaTezt  {

    type idapply[+a] = a

    trait Monad1[n[+_]]

    object Monad1 {
        implicit val ofWeakIdentity: Monad1[({type m[+a] = a})#m] = throw new Error
        implicit val ofWeakIdentity_ : Monad2[idapply] = throw new Error // wow, ok.
    }

    trait Monad2[n[+_]]

    object Monad2 {
        implicit val ofWeakIdentity: Monad2[idapply] = throw new Error
    }

    implicitly[Monad1[({type m[+a] = a})#m]]
    implicitly[Monad2[({type m[+a] = a})#m]]
    implicitly[Monad1[idapply]]
    implicitly[Monad2[idapply]]


}

