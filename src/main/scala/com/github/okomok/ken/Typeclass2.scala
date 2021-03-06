

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


trait Typeclass2[f[-_, +_]] extends TypeclassLike with Type2[f] with Kind.quote2[f] {
    /**
     * Helper for type-parameter inference
     */
    final def infer[a, b](x: f[a, b]): f[a, b] = x
}
