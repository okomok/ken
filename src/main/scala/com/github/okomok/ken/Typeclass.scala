

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Marker for typeclasses
 */
trait Typeclass


trait Typeclass0[a] extends Typeclass with Kind.Function0 {
    override type apply = a
}


trait Typeclass1[f[+_]] extends Typeclass with Kind.Function1 {
    override type apply[+a] = f[a]

    /**
     * Helper for type-parameter inference
     */
    final def infer[a](x: f[a]): f[a] = x

    /**
     * Helper for type-parameter inference
     */
    trait With1[f_ <: Kind.Function1] {
        // Workaround: java.lang.Error: unexpected alias type: type f
        protected type f[+a] = f_ #apply[a]
        protected type m[+a] = f[a]
    }

    def with1[f_ <: Kind.Function1]: With1[f_] = new With1[f_]{}
}


trait Typeclass2[f[-_, +_]] extends Typeclass with Kind.Function2 {
    override type apply2[-a, +b] = f[a, b]

    /**
     * Helper for type-parameter inference
     */
    final def infer[a, b](x: f[a, b]): f[a, b] = x

    /**
     * Helper for type-parameter inference
     */
    trait With1[f_ <: Kind.Function1] {
        // Workaround: java.lang.Error: unexpected alias type: type f
        protected type f[+a] = f_ #apply[a]
        protected type m[+a] = f[a]
    }

    def with1[f_ <: Kind.Function1]: With1[f_] = new With1[f_]{}
}
