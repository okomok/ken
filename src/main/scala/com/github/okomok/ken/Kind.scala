

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Type-level utilities
 */
object Kind {
    /**
     * Marker
     */
    trait FunctionLike

    /**
     * Variadic
     */
    trait FunctionV extends FunctionLike {
        type applyV[-a, +b]
    }

    trait Function0 extends FunctionV {
        type apply0
    }

    trait Function1 extends FunctionV {
        type apply1[+a]
        type apply[+a] // alias of apply1
    }

    trait Function2 extends FunctionV {
        type apply2[-a, +b]
    }

    trait AbstractFunction0 extends Function0  {
        override type applyV[-a, +b] = apply0
    }

    trait AbstractFunction1 extends Function1 {
        override type apply[+a] = apply1[a]
        override type applyV[-a, +b] = apply1[b]
    }

    trait AbstractFunction2 extends Function2 {
        override type applyV[-a, +b] = apply2[a, b]
    }

    trait Strong0 extends AbstractFunction0 {
        type weak0
    }

    trait Strong1 extends AbstractFunction1 {
        type weak1[+a]
    }

    trait MonadTrans extends Strong1 {
        type inner[+a]
    }

    trait const0[z] extends AbstractFunction0 {
        override type apply0 = z
    }

    trait const1[z] extends AbstractFunction1 {
        override type apply1[+a] = z
    }

    trait quote1[f[+_]] extends AbstractFunction1 {
        override type apply1[+a] = f[a]
    }

    trait qcurry2[f[_, +_]] extends FunctionLike {
        sealed trait apply[a] extends AbstractFunction1 {
            override type apply1[+b] = f[a, b]
        }
    }
}
