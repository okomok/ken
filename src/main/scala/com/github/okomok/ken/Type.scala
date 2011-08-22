

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


/**
 * Trival type envelope for type-parameter inference
 */
sealed abstract class Type[a] extends Kind.AbstractFunction0 {
    override type apply0 = a
}

object Type {
    def apply[a]: Type[a] = new Type[a]{}

    implicit def _ofAny[a]: Type[a] = new Type[a]{}
}
