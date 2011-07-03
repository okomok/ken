

// Copyright Shunsuke Sogame 2011.
// Distributed under the terms of an MIT-style license.


package com.github.okomok
package ken


object Test {
    import Applicative.function1

    val r = ((x: Int) => (y: Int) => x + y) <@> ((x: Int) => x)
}
