

// Copyright Shunsuke Sogame 2011.
// Distributed under the New BSD license.


package com.github.okomok
package ken


/**
 * Trivial type envelope for type-parameter inference
 */
sealed class Type[a] extends Kind.const[a]

object Type {
    def apply[a]: Type[a] = new Type[a]
}
