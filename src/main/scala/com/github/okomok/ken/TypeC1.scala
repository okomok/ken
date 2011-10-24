

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Type-constructor envelope for type-parameter inference
 */
trait TypeC1[f[_]]


object TypeC1 {
    def apply[f[_]]: TypeC1[f] = new TypeC1[f] {}
}
