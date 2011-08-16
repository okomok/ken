

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Type-level utilities
 */
object Kind {
    @Annotation.marker
    trait FunctionLike

    /**
     * Variadic
     */
    sealed trait FunctionV extends FunctionLike {
        type applyV[-a, +b]
    }

    // Functions
    //
    sealed trait Function0 extends FunctionV {
        type apply0
    }

    sealed trait Function1 extends FunctionV {
        type apply1[+a]
        type apply[+a] // alias of apply1
    }

    sealed trait Function2 extends FunctionV {
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

    // Newtypes
    //
    sealed trait Newtype0 extends Function0 {
        type oldtype0
    }

    trait AbstractNewtype0 extends Newtype0 with AbstractFunction0 {
    }

    sealed trait Newtype1 extends Function1 {
        type oldtype1[+a]
    }

    trait AbstractNewtype1 extends Newtype1 with AbstractFunction1 {
    }

    sealed trait Newtype2 extends Function2 {
        type oldtype2[-a, +b]
    }

    trait AbstractNewtype2 extends Newtype2 with AbstractFunction2 {
    }

    // MonadTrans
    //
    sealed trait MonadTrans extends Newtype1 {
        type innerMonad[+a]
    }

    trait AbstractMonadTrans extends MonadTrans with AbstractNewtype1 {
    }

    // Misc
    //
    trait const0[z] extends AbstractFunction0 {
        override type apply0 = z
    }

    trait const1[z] extends AbstractFunction1 {
        override type apply1[+a] = z
    }

    trait quote1[f[+_]] extends AbstractFunction1 {
        override type apply1[+a] = f[a]
    }

    trait quote2[f[-_, +_]] extends AbstractFunction2 {
        override type apply2[-a, +b] = f[a, b]
    }

    trait qcurry2[f[_, +_]] extends FunctionLike {
        sealed trait apply[a] extends AbstractFunction1 {
            override type apply1[+b] = f[a, b]
        }
    }
}
