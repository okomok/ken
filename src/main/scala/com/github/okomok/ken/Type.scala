

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
}


/*
better name needed

sealed abstract class Type1[f[+_]] extends Kind.quote1[f]

object Type1 {
    def apply[f[+_]]: Type1[f] = new Type1[f]{}
}

sealed abstract class Type2[f[+_, +_]] extends Kind.quote2[f]

object Type2 {
    def apply[f[+_, +_]]: Type2[f] = new Type2[f]{}
}
*/
