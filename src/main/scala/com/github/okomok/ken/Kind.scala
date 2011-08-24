

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Type-level utilities
 */
object Kind {
    @Annotation.marker
    trait FunctionLike

    // Functions
    //
    sealed trait Function0 extends FunctionLike {
        type apply0
    }

    sealed trait Function1 extends FunctionLike {
        type apply1[+a]
        type apply[+a] // alias of apply1
    }

    sealed trait Function2 extends FunctionLike {
        type apply2[-a, +b]
    }

    trait AbstractFunction0 extends Function0  {
    }

    trait AbstractFunction1 extends Function1 {
        override type apply[+a] = apply1[a]
    }

    trait AbstractFunction2 extends Function2 {
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
    trait const[z] extends AbstractFunction0 with AbstractFunction1 with AbstractFunction2 {
        override type apply0 = z
        override type apply1[+a] = z
        override type apply2[-a, +b] = z
    }

    // needed for variance-correct
    trait constThis extends AbstractFunction0 with AbstractFunction1 with AbstractFunction2 {
        override type apply0 = this.type
        override type apply1[+a] = this.type
        override type apply2[-a, +b] = this.type
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
