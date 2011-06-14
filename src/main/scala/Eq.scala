

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


// Works around name-collision against Ordering EQ.
// Note toplevel identifiers are case-insensitive wrt file-system.
private[ken] object _Eq {
    trait Eq[a]
    object Eq {
        def op_==[a](x: a)(y: a): Boolean = x == y
        def op_/=[a](x: a)(y: a): Boolean = x != y
    }
}
