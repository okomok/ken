

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Typeclass1[f[+_]] extends TypeclassLike with Type1[f] with Kind.quote1[f] {

    /**
     * Helper for type-parameter inference
     */
    final def infer[a](x: f[a]): f[a] = x

    // @deprecated
    trait Pull[f_ <: Kind.Function1] {
        // Workaround: https://issues.scala-lang.org/browse/SI-4312
        protected[this] type f[+a] = f_ #apply1[a]
        protected[this] type m[+a] = f[a]
    }
    def pull[f_ <: Kind.Function1]: Pull[f_] = new Pull[f_] {}
}
