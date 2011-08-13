

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Will probably removed.

object Weak0 {
    def apply[p <: Kind.Strong0](implicit i: Imply0[p#apply0, p#weak0]): Imply0[p#apply0, p#weak0] = i
}

object Weak1 {
    def apply[p <: Kind.Strong1](implicit i: Imply1[p#apply1, p#weak1]): Imply1[p#apply1, p#weak1] = i
}
