

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Trival type envelope for type-parameter inference
 */
sealed class Type[a] extends Kind.const[a]

object Type {
    def apply[a]: Type[a] = new Type[a]
}
