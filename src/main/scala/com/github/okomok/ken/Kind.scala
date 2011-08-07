

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Kind {
    trait Function0 {
        type apply
    }

    trait Function1 {
        type apply[+a]
    }

    trait Function1nv {
        type apply[a]
    }

    trait Function2 {
        type apply[+a, +b]
    }

    trait Strong0 extends Function0 {
        type weak
    }

    trait Strong1 extends Function1 {
        type weak[+a]
    }

    trait MonadTrans extends Strong1 {
        type inner[+a]
    }

    trait quote1[f[+_]] extends Function1 {
        override type apply[+a] = f[a]
    }

    trait curry2[f[_, +_]] extends Function1nv {
        sealed trait apply[a] extends Function1 {
            override type apply[+b] = f[a, b]
        }
    }
}
