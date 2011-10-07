

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Type-constructor envelope for type-parameter inference
 */
trait TypeC2[f[-_, +_]] extends Kind.quote2[f]


object TypeC2 {
    def apply[f[-_, +_]]: TypeC2[f] = new TypeC2[f] {}
}
