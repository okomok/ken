

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Type-level utilities
 */
object Kind {
    /**
     * Marker trait for kind functions
     */
    trait Function

    trait Function0 extends Function {
        type apply
    }

    trait Function1 extends Function {
        type apply[+a]
    }

    trait Function2 extends Function {
        type apply[-a, +b]
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

    trait const0[z] extends Function0 {
        override type apply = z
    }

    trait const1[z] extends Function1 {
        override type apply[+a] = z
    }

    trait const2[z] extends Function2 {
        override type apply[-a, +b] = z
    }

    trait quote1[f[+_]] extends Function1 {
        override type apply[+a] = f[a]
    }

    trait quote2[f[-_, +_]] extends Function2 {
        override type apply[-a, +b] = f[a, b]
    }

    trait qcurry2[f[_, +_]] extends Function {
        sealed trait apply[a] extends Function1 {
            override type apply[+b] = f[a, b]
        }
    }
}
