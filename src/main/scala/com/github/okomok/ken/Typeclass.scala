

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


@Annotation.marker
trait Typeclass


trait Typeclass0[a] extends Typeclass with Kind.AbstractFunction0 {
    override type apply0 = a

    // Newtypes
    //
    def deriving[nt](implicit i: Newtype0[nt, a]): Typeclass0[nt] = error("abstract me")
}


trait Typeclass1[f[+_]] extends Typeclass with Kind.AbstractFunction1 {
    override type apply1[+a] = f[a]

    /**
     * Helper for type-parameter inference
     */
    final def infer[a](x: f[a]): f[a] = x

    /**
     * Helper for type-parameter inference
     */
    trait Pull[f_ <: Kind.FunctionV] {
        // Workaround: https://issues.scala-lang.org/browse/SI-4312
        protected[this] type f[+a] = f_ #applyV[_, a]
        protected[this] type m[+a] = f[a]
    }
    def pull[f_ <: Kind.FunctionV]: Pull[f_] = new Pull[f_]{}
}


trait Typeclass2[f[-_, +_]] extends Typeclass with Kind.AbstractFunction2 {
    override type apply2[-a, +b] = f[a, b]

    /**
     * Helper for type-parameter inference
     */
    final def infer[a, b](x: f[a, b]): f[a, b] = x
}
