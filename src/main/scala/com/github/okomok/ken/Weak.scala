

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Weak0 {
    def apply[nt <: Kind.Newtype0](implicit i: Newtype0[nt#apply0, nt#oldtype0]): Newtype0[nt#apply0, nt#oldtype0] = i
}

/*
object Weak1 {
    def apply[p <: Kind.Strong1](implicit i: Imply1[p#apply1, p#weak1]): Imply1[p#apply1, p#weak1] = i
}
*/
